with VariableStore;
with PIN;
with MyExceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Latin_1;
with Ada.Containers.Formal_Ordered_Maps;
with Ada.Containers; use Ada.Containers;
with Ada.Strings.Fixed;
use Ada.Strings.Fixed;
with Ada.Characters.Latin_1;
generic
    Max_Size : Positive;

package MyCalculator with SPARK_Mode is
    type MyCalculator is private;
    subtype Item is Integer;

    ------------------- Public Procedures -------------------
    -- initializes the calculator with the given master PIN.

    procedure Init(C : out MyCalculator; VarDb : in VariableStore.Database ;MasterPINString : in String) with
        -- 1. pin is valid;
        Pre => IsPin(MasterPINString),
        -- 1. stack is empty; 2. pin is set to the given pin; 3. calculator is locked.
        Post => Size(C) = 0 
            and PIN."="(PIN.From_String(MasterPINString), GetPin(C))
            and IsLocked(C) = True and VariableStore.Length(GetVarDb(C))'Image = "0";

    -- unlock the calculator with the given PIN.
    procedure Unlock(C : in out MyCalculator; PINString : in String) with
        -- 1. pin is valid; 2. calculator is locked.
        Pre => IsPin(PINString) and IsLocked(C) = True,
        -- 1. calculator is unlocked; 2. pin remains unchanged.
        Post => ((PIN."="(PIN.From_String(PINString), GetPin(C))) and IsLocked(C) = False 
                    and PIN."="(GetPin(C), GetPin(C'Old))) 
                    -- 2. if password wrong, remains locked
                    or (IsLocked(C) = True and not PIN."="(GetPin(C), GetPin(C'Old)));

    -- lock and update the master pin.
    procedure Lock(C : in out MyCalculator; PINString : in String) with
        -- 1. pin is valid; 2. calculator is unlocked.
        Pre => IsPin(PINString) and IsLocked(C) = False,
        -- 1. calculator is locked; 2. pin is set to the given pin.
        Post => IsLocked(C) = True and PIN."="(PIN.From_String(PINString), GetPin(C));

    -- pushes the number value onto operand stack.
    procedure PushNumber(C : in out MyCalculator; NumIn : in Item) with
        -- 1. stack is not full; 2. calculator is unlocked.
        Pre => Size(C) < Max_Size and IsLocked(C) = False,
        -- 1. stack size increased; 2. the top of the stack is the pushed number.
        -- 3. other elements remain unchanged.
        Post => Size(C) = Size(C'Old) + 1 and Storage(C, Size(C)) = NumIn
            and (for all I in 1..Size(C'Old) => Storage(C, I) = Storage(C'Old, I))
            and IsLocked(C) = IsLocked(C'Old);

    -- pops the value from the top of the operand stack, discarding it.
    procedure PopNumber(C : in out MyCalculator; NumOut : out Item) with
        -- 1. stack is not empty; 2. calculator is unlocked.
        Pre => Size(C) > 0 and IsLocked(C) = False,
        -- 1. stack size decreased; 2. the popped number is the top of the stack.
        -- 3. other elements remain unchanged.
        Post => Size(C) = Size(C'Old) - 1 and NumOut = Storage(C'Old, Size(C'Old))
            and (for all I in 1..Max_Size => Storage(C, I) = Storage(C'Old, I))
            and IsLocked(C) = IsLocked(C'Old);

    -- The commands “+”, “-”, “*” and “/” each pop the top two values 
    -- from the operand stack and compute the corresponding arithmetic 
    -- operation on them (addition, subtraction, multiplication and division, respectively), 
    -- and push the result onto the stack.
    procedure PerformOperation(C : in out MyCalculator; 
                               Operator : in String; 
                               NumOut : out Item) with
        Pre => IsLocked(C) = False and IsValidOperator(Operator) and Size(C) >= 2,
        Post => (Size(C) = Size(C'Old) - 1 and NumOut = Storage(C, Size(C))
            and IsLocked(C) = IsLocked(C'Old)
            and (for all I in 1..(Size(C) - 1) => Storage(C, I) = Storage(C'Old, I)))
                -- if result overflow, then the stack remains unchanged
                or (Size(C) = Size(C'Old) and (for all I in 1..Size(C) => Storage(C, I) = Storage(C'Old, I)) and NumOut = 0);

    -- For a string var, the procedure loads the value stored 
    -- in variable var and pushes it onto the stack.
    procedure LoadVar(C : in out MyCalculator; VarString: in String; Var : out VariableStore.Variable) with
        Pre => IsLocked(C) = False and IsValidVarName(VarString) and Size(C) < Max_Size,
        Post => (Size(C) = Size(C'Old) + 1 and Storage(C, Size(C)) = VariableStore.Get(GetVarDb(C), Var)
            and (for all I in 1..Size(C'Old) => Storage(C, I) = Storage(C'Old, I)) and IsLocked(C) = IsLocked(C'Old))
            -- if variable not found, then the stack remains unchanged
            or (Size(C) = Size(C'Old) 
                and (for all I in 1..Size(C) => Storage(C, I) = Storage(C'Old, I))
                and IsLocked(C) = IsLocked(C'Old)
                );

    -- pops the value from the top of the stack and stores it 
    -- into variable var, defining that variable if it is not already defined.
    procedure StoreVar(C : in out MyCalculator; VarString: in String; Var : out VariableStore.Variable) with
        Pre => IsLocked(C) = False and IsValidVarName(VarString) and Size(C) > 0,
        Post => ((Size(C) = Size(C'Old) - 1 
            and (for all J in 1..Max_Size => Storage(C,J) = Storage(C'Old,J))
            and VariableStore.Has_Variable(GetVarDb(C), VariableStore.From_String(VarString)) 
            and VariableStore.Get(GetVarDb(C), Var) = Storage(C'Old, Size(C'Old))))
            and IsLocked(C) = IsLocked(C'Old);
            
    -- makes variable var undefined (i.e. it will not be printed by subsequent “list” commands).
    procedure RemoveVar(C : in out MyCalculator; VarString: String; Var : out VariableStore.Variable) with
        Pre => IsLocked(C) = False and then IsValidVarName(VarString),
        Post => (Size(C) = Size(C'Old) 
            and (for all J in 1..Max_Size => Storage(C,J) = Storage(C'Old,J)) 
            and not VariableStore.Has_Variable(GetVarDb(C), Var))
            and IsLocked(C) = IsLocked(C'Old);

    ------------------- Utils -------------------
     -- helper ghost function for checking
    function Storage(C : in MyCalculator; Pos : in Integer) return Item with
     Ghost,
     Pre => Pos >= 1 and Pos <= Max_Size;

    -- check if the calculator is locked or not
    function IsLocked(C : in MyCalculator) return Boolean;

    -- get the size of the stack
    function Size(C : in MyCalculator) return Integer;  

    -- get the pin of the calculator
    function GetPin(C : in MyCalculator) return PIN.PIN;

    -- get var db
    function GetVarDb(C : in MyCalculator) return VariableStore.Database;

    -- helper function for checking if the string is a valid number
    function IsNumber (S : in String) return Boolean;
   
    -- check if the string is a valid pin, is a 4-digit string of 
    -- non-whitespace characters that represents a non-negative number 
    -- (i.e. a natural number) in the range 0000 . . . 9999. 
    function IsPin (S : in String) return Boolean;

    -- check if the string is a valid operator
    function IsValidOperator(S : in String) return Boolean;

    -- check if the string is a valid Var name, which is the 
    -- string of non-whitespace characters, and names longer 
    -- than 1024 characters are invalid.
    function IsValidVarName(S : in String) return Boolean;

    -- check if the string is a valid command for the calc
   function IsValidCommand (S : in String) return Boolean;

private
    type StorageArray is array (Integer range 1..Max_Size) of Item;
    type MyCalculator is record
        isLocked : Boolean;
        masterPin : PIN.PIN;
        storage : StorageArray;
        size : Integer range 0..Max_Size;
        VariableDB : VariableStore.Database;
    end record;

    ------------------- Util implementation-------------------
    -- help function for getting stack top
    function Size(C : in MyCalculator) return Integer is
        (C.size);

    -- helper function for get the value of the stack storage
    function Storage(C : in MyCalculator; Pos : in Integer) return Item is
        (C.storage(Pos));

    -- Get whether is locked or not
    function IsLocked(C : in MyCalculator) return Boolean is
        (C.isLocked);

    -- helper function for checking if the string is a valid number
    function IsNumber (S: in String) return Boolean is
        (for all I in S'Range => 
            S(I) >= '0' and S(I) <= '9');
   
    -- check if the string is a valid pin, is a 4-digit string of 
    -- non-whitespace characters that represents a non-negative number 
    -- (i.e. a natural number) in the range 0000 . . . 9999. 
    function IsPin (S : in String) return Boolean is
      (S'Length = 4 and Index_Non_Blank (S) /= 0 and (for all I in S'Range 
            => S(I) /= ' ' and S(I) /= Ada.Characters.Latin_1.NUL) and IsNumber(S)); 

    -- check if the string is a valid operator
    function IsValidOperator(S : in String) return Boolean is
        (S = "+" or S = "-" or S = "*" or S = "/");

    -- check if the string is a valid Var name, which is the 
    -- string of non-whitespace characters, and names longer 
    -- than 1024 characters are invalid.
    function IsValidVarName(S : in String) return Boolean is
        (S'Length <= VariableStore.Max_Variable_Length and S'Length > 0 
        and Index_Non_Blank (S) /= 0 and (for all I in S'Range 
            => S(I) /= ' ' and S(I) /= Ada.Characters.Latin_1.NUL));

    -- check if the string is a valid command for the calc
    function IsValidCommand (S : in String) return Boolean is
       (S = "push" or S = "pop" or S = "load" 
       or S = "store" or S = "remove" or S = "lock"
       or S = "unlock" or S = "list");

    -- get tgh pin of the calculator
    function GetPin(C : in MyCalculator) return PIN.PIN is
        (C.masterPin);

    -- get var db
    function GetVarDb(C : in MyCalculator) return VariableStore.Database is
        (C.VariableDB);

end MyCalculator;
