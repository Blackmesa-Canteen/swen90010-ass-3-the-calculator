package body MyCalculator with SPARK_Mode is

    -- Init the calc
    procedure Init(C : out MyCalculator; MasterPINString : in String) is
    begin
        -- default is locked
        C.isLocked := True;

        -- check valid pin string
        if not IsPin(MasterPINString) then
            raise PIN_Exception with "PIN should be 0000 . . . 9999. ";
        end if;
        C.MasterPIN := PIN.From_String(MasterPINString);

        -- point to stack bottom
        C.size := 0;
        -- init stack array
        C.storage := (others => Default_Item);

        -- init variable storage
        C.variableDB := VariableStore.Database;
        VariableStore.Init(C.variableDB);

    end Init;

    -- try unlock. 
    procedure Unlock(C : in out MyCalculator; PINString : in String)is
    begin
        -- check whether the string is valid
        if not IsPin(PINString) then
            raise PIN_Exception with "PIN should be 0000 . . . 9999.";
        end if;

        -- if locked, compare password and try to unlock
        if ((C.isLocked = True) and then PIN."="(PIN.From_String(PINString), C.MasterPIN)) then
            C.isLocked := False;
        end if;

    end Unlock;

    -- try lock
    procedure Lock(C : in out MyCalculator; PINString : in String) is
    begin
        -- check whether the string is valid
        if not IsPin(PINString) then
            raise PIN_Exception with "PIN should be 0000 . . . 9999. ";
        end if;

        if (C.isLocked = False) then
            -- set a new master pin, and then lock the calculator
            C.MasterPIN := PIN.From_String(PINString);
            C.isLocked := True;
        end if;
    end Lock;

    -- Get whether is locked or not
    function GetLockState(C : in MyCalculator) return Boolean is
    begin
        return C.isLocked;
    end GetLockState;

    -- Push a number in
    procedure PushNumber(C : in out MyCalculator; NumIn : in Item) is
    begin
        -- check lock state
        if (C.isLocked = True) then
            raise Lock_Exception with "Calculator is locked.";
        else
            -- handle int overflow: Decimal integers in commands 
            -- (e.g. “5” in “push 5”) that are outside the range 
            -- −2^31. . . 2^31-1 inclusive are treated as 0.
            if not IsValidInt(NumIn) then
                NumIn := 0;
            end if;

            -- check whether the stack is full
            if (C.size >= Max_Size) then
                raise Stack_Exception with "Stack is full.";
            end if;

            C.size := C.size + 1;
            C.storage(C.size) := NumIn;
        end if;
    end PushNumber;

    -- pop a number out
    procedure PopNumber(C : in out MyCalculator; NumOut : out Item) is
    begin
        -- check lock state
        if (C.isLocked = True) then
            raise Lock_Exception with "Calculator is locked.";
        else
            -- check whether the stack is empty
            if (C.size <= 0) then
                raise Stack_Exception with "Stack is empty.";  
            end if;

            NumOut := C.storage(C.size);
            C.size := C.size - 1;   
        end if;
    end PopNumber;

    -- The commands “+”, “-”, “*” and “/” each pop the top two values 
    -- from the operand stack and compute the corresponding arithmetic 
    -- operation on them (addition, subtraction, multiplication and division, respectively), 
    -- and push the result onto the stack.
    procedure PerformOperation(C : in out MyCalculator; 
                               Operator : in String; 
                               NumOut : out Item) is
    begin
        -- check lock state
        if (C.isLocked = True) then
            raise Lock_Exception with "Calculator is locked.";
        else
            -- check whether the operator is valid
            if not IsValidOperator(Operator) then
                raise Operator_Exception with "Operator should be +, -, *, /";
            end if;

            -- check whether the stack has enough elements
            if (C.size < 2) then
                raise Stack_Exception with "Stack has less than 2 elements for calculation.";
            end if;

            -- pop the top two values from the operand stack
            declare 
                Num1 : Item;
                Num2 : Item;
                IsNum1Possitive : Boolean;
                IsNum2Possitive : Boolean;
                IsProductPossitive: Boolean;
                Max_Integer : constant Integer := Integer'Last;
                Min_Integer : constant Integer := Integer'First;
            begin
                -- pop out the number
                PopNumber(C, Num1);
                PopNumber(C, Num2);

                -- check whether the number is possitive
                IsNum1Possitive := (Num1 >= 0);
                IsNum2Possitive := (Num2 >= 0);

                -- check whether the product is possitive or not
                IsProductPossitive := (IsNum1Possitive and IsNum2Possitive) or 
                                      (not IsNum1Possitive and not IsNum2Possitive);

                 -- compute the corresponding arithmetic operation on them
                case Operator is
                    when "+" =>
                        -- check addition positive overflow
                        if (IsNum2Possitive and Num1 > Max_Integer - Num2) then
                            raise Calc_Exception with "Addition overflow.";
                        end if;

                        -- check addition negative overflow
                        if (not IsNum2Possitive and Num1 < Min_Integer - Num2) then
                            raise Calc_Exception with "Addition overflow.";
                        end if;

                        PushNumber(C, NumOut + NumOut);
                    when "-" =>
                        -- check subtraction positive overflow
                        if (not IsNum2Possitive and Num1 > Max_Integer + Num2) then
                            raise Calc_Exception with "Subtraction overflow.";
                        end if;

                        -- check substraction negative overflow
                        if (IsNum2Possitive and Num1 < Min_Integer + Num2) then
                            raise Calc_Exception with "Subtraction overflow.";
                        end if;

                        PushNumber(C, NumOut - NumOut);
                    when "*" =>
                        -- check multiplication possitive overflow
                        if (IsProductPossitive and Num1 > Max_Integer / Num2) then
                            raise Calc_Exception with "Multiplication overflow.";
                        end if;

                        -- check multiplication negative overflow
                        if (not IsProductPossitive and Num1 < Min_Integer / Num2) then
                            raise Calc_Exception with "Multiplication overflow.";
                        end if;

                        PushNumber(C, NumOut * NumOut);
                    when "/" =>
                        -- check divide 0
                        if (Num2 = 0) then
                            raise Calc_Exception with "Divide 0.";
                        end if;

                        -- check multiplication overflow
                        if (Num1 = Max_Integer and Num2 = -1) then
                            raise Calc_Exception with "Division overflow.";
                        end if;

                        PushNumber(C, Num1 / Num2);
                end case; 

            -- If calc exception is raised, then push the number back
            exception
                when Calc_Exception =>
                    PushNumber(C, Num2);
                    PushNumber(C, Num1);
                    -- continuing throw up the exception
                    raise;
            end;      
        end if;
    end PerformOperation;


    -- For a string var, the procedure loads the value stored 
    -- in variable var and pushes it onto the stack.
    procedure loadVar(C : in out MyCalculator; VarString: in String) is
    begin
        -- check lock state
        if (C.isLocked = True) then
            raise Lock_Exception with "Calculator is locked.";
        else
            -- check whether the string is valid
            if not IsValidVarName(VarString) then
                raise Var_Exception with "Variable name is invalid.";
            end if;

            declare
                V : VariableStore.Variable;
                Num : Item;
            begin
                -- check whether the variable exists
                V := VariableStore.From_String(VarString);
                if not VariableStore.Has_Variable(C.variableDB, V) then
                    raise Var_Exception with "Variable does not exist.";
                else
                    -- If exist, push the value onto the stack
                    Num := VariableStore.Get(C.variableDB, V);
                    PushNumber(C, Num);
                end if;
            end;
            
        end if;
    end loadVar;

    -- pops the value from the top of the stack and stores it 
    -- into variable var, defining that variable if it is not already defined.
    procedure storeVar(C : in out MyCalculator; VarString: in String) is
    begin
        -- check lock state
        if (C.isLocked = True) then
            raise Lock_Exception with "Calculator is locked.";
        else
            -- check whether the string is valid
            if not IsValidVarName(VarString) then
                raise Var_Exception with "Variable name is invalid.";
            end if;

            declare
                V : VariableStore.Variable;
                Num : Item;
            begin
                -- pop the value from the top of the stack
                PopNumber(C, Num);

                -- store the value into variable var
                V := VariableStore.From_String(VarString);
                VariableStore.Put(C.variableDB, V, Num);
            end;
        end if;
    end storeVar;

    -- makes variable var undefined (i.e. it will not be printed by subsequent “list” commands).
    procedure removeVar(C : in out MyCalculator; VarString: String) is 
    begin
        -- check lock state
        if (C.isLocked = True) then
            raise Lock_Exception with "Calculator is locked.";
        else
            -- check whether the string is valid
            if not IsValidVarName(VarString) then
                raise Var_Exception with "Variable name is invalid.";
            end if;

            declare
                V : VariableStore.Variable;
            begin
                -- check whether the variable exists
                V := VariableStore.From_String(VarString);
                if not VariableStore.Has_Variable(C.variableDB, V) then
                    raise Var_Exception with "Variable does not exist.";
                else
                    -- remove the variable
                    VariableStore.Remove(C.variableDB, V);
                end if;
            end;
        end if;
    end removeVar;

    -- prints out all currently defined variables and their corresponding values.
    procedure list(C : in MyCalculator) is
    begin
        -- check lock state
        if (C.isLocked = True) then
            raise Lock_Exception with "Calculator is locked.";
        else
            -- print out all currently defined variables and their corresponding values
            VariableStore.Print(C.variableDB);
        end if;
    end list;

end MyCalculator;