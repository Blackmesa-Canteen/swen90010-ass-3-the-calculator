with VariableStore;

generic
    Max_Size : Positive;
    type Item is private;
    Default_Item : Item;

package MyCalculator with SPARK_Mode is
    type MyCalculator is private;

    procedure Init(C : out MyCalculator; MasterPIN : in String);

    function TryUnlockByPIN(C : in out MyCalculator; PIN : in String) return Boolean;

    function GetLockState(C : in MyCalculator) return Boolean;

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
    procedure loadVar(C : in out MyCalculator; Var: in String);

    -- pops the value from the top of the stack and stores it 
    -- into variable var, defining that variable if it is not already defined.
    procedure storeVar(C : in out MyCalculator; Var: in String);

    -- makes variable var undefined (i.e. it will not be printed by subsequent “list” commands).
    procedure removeVar(C : in out MyCalculator; Var: String);

    -- prints out all currently defined variables and their corresponding values.
    procedure list(C : in MyCalculator);


private
    type StorageArray is array (Integer range 1..Max_Size) of Item;
    type MyCalculator is record
        isLocked : Boolean := False;
        masterPin : String;
        storage : StorageArray;
        size : Integer range 0..Max_Size;
        variableDB : VariableStore.Database;
        end record;

    function Size(C : in MyCalculator) return Integer is
        (C.size);

    function Storage(C : in MyCalculator; Pos : in Integer) return Item is
        (C.storage(Pos));

    function GetLockState(C : in MyCalculator) return Boolean is
        (C.isLocked);

end MyCalculator;
