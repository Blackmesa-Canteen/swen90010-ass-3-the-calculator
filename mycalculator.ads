package MyCalculator with SPARK_Mode is
    type Object is tagged private;

    procedure TryUnlockByPIN(PIN : String) return Boolean;
    procedure ResetPIN(NewPIN : String);

    -- pushes the number value onto operand stack.
    procedure PushNumber(Number : Integer);

    -- pops the value from the top of the operand stack, discarding it.
    procedure PopNumber return Integer;

    -- The commands “+”, “-”, “*” and “/” each pop the top two values 
    -- from the operand stack and compute the corresponding arithmetic 
    -- operation on them (addition, subtraction, multiplication and division, respectively), 
    -- and push the result onto the stack.
    procedure PerformOperation(Operator : String);

    -- For a string var, the procedure loads the value stored 
    -- in variable var and pushes it onto the stack.
    procedure loadVar(Var: String);

    -- pops the value from the top of the stack and stores it 
    -- into variable var, defining that variable if it is not already defined.
    procedure storeVar(Var: String);

    -- makes variable var undefined (i.e. it will not be printed by subsequent “list” commands).
    procedure removeVar(Var: String);

    -- list
    -- TODO list return type?


private
    type Object is tagged
        record
        IsLocked : Boolean := True;
        end record;

end MyCalculator;
