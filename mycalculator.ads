with VariableStore;
with PIN;
with MyExceptions;

generic
    Max_Size : Positive;
    type Item is private;
    Default_Item : Item;

package MyCalculator with SPARK_Mode is
    type MyCalculator is private;

    -- public functions -------------------------------------------------------

    -- initializes the calculator with the given master PIN.
    procedure Init(C : out MyCalculator; MasterPINString : in String);

    -- unlock the calculator with the given PIN.
    procedure Unlock(C : in out MyCalculator; PINString : in String);

    -- lock and update the master pin.
    procedure Lock(C : in out MyCalculator; PINString : in String);

    -- check if the calculator is locked or not
    function IsLocked(C : in MyCalculator) return Boolean;

    -- get the size of the stack
    function Size(C : in MyCalculator) return Integer;

    -- helper ghost function for checking
    function Storage(S : in MyCalculator; Pos : in Integer) return Item with
     Ghost,
     Pre => Pos >= 1 and Pos <= Max_Size;

    -- pushes the number value onto operand stack.
    procedure PushNumber(C : in out MyCalculator; NumIn : in Item);

    -- pops the value from the top of the operand stack, discarding it.
    procedure PopNumber(C : in out MyCalculator; NumOut : out Item);

    -- The commands “+”, “-”, “*” and “/” each pop the top two values 
    -- from the operand stack and compute the corresponding arithmetic 
    -- operation on them (addition, subtraction, multiplication and division, respectively), 
    -- and push the result onto the stack.
    procedure PerformOperation(C : in out MyCalculator; 
                               Operator : in String; 
                               NumOut : out Item);

    -- For a string var, the procedure loads the value stored 
    -- in variable var and pushes it onto the stack.
    procedure LoadVar(C : in out MyCalculator; VarString: in String);

    -- pops the value from the top of the stack and stores it 
    -- into variable var, defining that variable if it is not already defined.
    procedure StoreVar(C : in out MyCalculator; VarString: in String);

    -- makes variable var undefined (i.e. it will not be printed by subsequent “list” commands).
    procedure RemoveVar(C : in out MyCalculator; VarString: String);

    -- prints out all currently defined variables and their corresponding values.
    procedure List(C : in MyCalculator);


private
    type StorageArray is array (Integer range 1..Max_Size) of Item;
    type MyCalculator is record
        isLocked : Boolean := False;
        masterPin : PIN.PIN;
        storage : StorageArray;
        size : Integer range 0..Max_Size;
        variableDB : VariableStore.Database;
        end record;

    -- private Utils ----------------------------------------------------------

    -- help function for getting stack top
    function Size(C : in MyCalculator) return Integer is
        (C.size);

    -- helper function for get the value of the stack storage
    function Storage(C : in MyCalculator; Pos : in Integer) return Item is
        (C.storage(Pos));

    -- helper function for checking if the calculator is locked or not
    function GetLockState(C : in MyCalculator) return Boolean is
        (C.isLocked);

    -- helper function for checking if the string is a valid number
    function IsNumber (S:String) return Boolean is
        (for all I in S'Range => 
            S(I) >= '0' and S(I) <= '9');
   
    -- check if the string is a valid pin, is a 4-digit string of 
    -- non-whitespace characters that represents a non-negative number 
    -- (i.e. a natural number) in the range 0000 . . . 9999. 
    function IsPin (S : String) return Boolean is
        (S'Length = 4 and IsNumber(S));

    -- check if the string is a valid integer
    function IsValidInt(V : Item) return Boolean is
        (V  >= Integer'First and V <= Integer'Last);

    -- check if the string is a valid operator
    function IsValidOperator(S : String) return Boolean is
        (S = "+" or S = "-" or S = "*" or S = "/");

    -- check if the string is a valid Var name, which is the 
    -- string of non-whitespace characters, and names longer 
    -- than 1024 characters are invalid.
    function IsValidVarName(S : String) return Boolean is
        (S'Length <= 1024 and for all I in S'Range 
            => S(I) /= ' ');

end MyCalculator;
