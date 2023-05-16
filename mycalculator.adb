with Ada.Text_IO; use Ada.Text_IO;
package body MyCalculator with SPARK_Mode is

    -- Init the calc
    procedure Init(C : out MyCalculator; MasterPINString : in String) is
    begin
        -- default is locked
        C.isLocked := True;

        -- check valid pin string
        if not IsPin(MasterPINString) then
            Put_Line("PIN_Exception: PIN should be 0000 . . . 9999.");
        end if;
        C.MasterPIN := PIN.From_String(MasterPINString);

        -- point to stack bottom
        C.size := 0;
        -- init stack array
        C.storage := (others => 0);

        -- init variable storage
        VariableStore.Init(C.VariableDB);

    end Init;

    -- try unlock. 
    procedure Unlock(C : in out MyCalculator; PINString : in String)is
    begin
        -- check whether the string is valid
        if not IsPin(PINString) then
            Put_Line("PIN_Exception: PIN should be 0000 . . . 9999.");
        end if;

        if (C.isLocked = False) then
            -- already unlocked, throw exception to print something
            Put_Line("Lock_Exception: Already unlocked.");
        else
            -- if locked, compare password and try to unlock
            if (PIN."="(PIN.From_String(PINString), C.MasterPIN)) then
                C.isLocked := False;
            else
                -- wrong password
                Put_Line("Lock_Exception: Password is wrong.");
            end if;
        end if;

    end Unlock;

    -- try lock
    procedure Lock(C : in out MyCalculator; PINString : in String) is
    begin
        -- check whether the string is valid
        if not IsPin(PINString) then
            Put_Line("PIN_Exception: PIN should be 0000 . . . 9999. ");
        end if;

        if (C.isLocked = False) then
            -- set a new master pin, and then lock the calculator
            C.MasterPIN := PIN.From_String(PINString);
            C.isLocked := True;
        else
            -- already locked, throw exception to print something
            Put_Line("Lock_Exception: Already locked.");
        end if;
    end Lock;

    -- Get whether is locked or not
    function IsLocked(C : in MyCalculator) return Boolean is
    begin
        return C.isLocked;
    end IsLocked;

    -- Push a number in
    procedure PushNumber(C : in out MyCalculator; NumIn : in Item) is
    begin
        -- check lock state
        if (C.isLocked = True) then
            Put_Line("Lock_Exception: Calculator is locked.");
        else
            -- check whether the stack is full
            if (C.size >= Max_Size) then
                Put_Line("Stack_Exception: Stack is full.");
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
            Put_Line("Lock_Exception: Calculator is locked.");
        else
            -- check whether the stack is empty
            if (C.size <= 0) then
                Put_Line("Stack_Exception: Stack is empty.");  
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
            Put_Line("Lock_Exception: Calculator is locked.");
        else
            -- check whether the operator is valid
            if not IsValidOperator(Operator) then
                Put_Line("Operator_Exception: Operator should be +, -, *, /");
            end if;

            -- check whether the stack has enough elements
            if (C.size < 2) then
                Put_Line("Stack_Exception: Stack has less than 2 elements for calculation.");
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
                Temp_R : Long_Long_Integer := 0;
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
                if Operator = "+" then
                     -- check addition overflow
                        Temp_R := Long_Long_Integer(num1) + Long_Long_Integer(num2);
                        if (Temp_R > Long_Long_Integer(Max_Integer) or 
                            Temp_R < Long_Long_Integer(Min_Integer)) then
                            Put_Line("Calc_Exception: Addition overflow.");
                        end if;

                        -- check addition positive overflow
                        --  if (IsNum2Possitive and Num1 > Max_Integer - Num2) then
                        --      raise MyExceptions.Calc_Exception with "Addition overflow.";
                        --  end if;

                        -- check addition negative overflow
                        --  if (not IsNum2Possitive and Num1 < Min_Integer - Num2) then
                        --      raise MyExceptions.Calc_Exception with "Addition overflow.";
                        --  end if;

                        NumOut := Num1 + Num2;
                        PushNumber(C, NumOut);
                elsif Operator = "-" then
                    -- check substraction overflow
                        Temp_R := Long_Long_Integer(num1) - Long_Long_Integer(num2);
                        if (Temp_R > Long_Long_Integer(Max_Integer) or 
                            Temp_R < Long_Long_Integer(Min_Integer)) then
                            Put_Line("Calc_Exception: Substraction overflow.");
                        end if;

                        -- check subtraction positive overflow
                        --  if (not IsNum2Possitive and Num1 > Max_Integer + Num2) then
                        --      raise MyExceptions.Calc_Exception with "Subtraction overflow.";
                        --  end if;

                        -- check substraction negative overflow
                        --  if (IsNum2Possitive and Num1 < Min_Integer + Num2) then
                        --      raise MyExceptions.Calc_Exception with "Subtraction overflow.";
                        --  end if;

                        NumOut := Num1 - Num2;
                        PushNumber(C, NumOut);
                elsif Operator = "*" then
                    -- check multiplication overflow
                        Temp_R := Long_Long_Integer(num1) * Long_Long_Integer(num2);
                        if (Temp_R > Long_Long_Integer(Max_Integer) or 
                            Temp_R < Long_Long_Integer(Min_Integer)) then
                            Put_Line("Calc_Exception: Multiplication overflow.");
                        end if;

                        -- check multiplication possitive overflow
                        --  if (IsProductPossitive and Num1 > Max_Integer / Num2) then
                        --      raise MyExceptions.Calc_Exception with "Multiplication overflow.";
                        --  end if;

                        -- check multiplication negative overflow
                        --  if (not IsProductPossitive and Num1 < Min_Integer / Num2) then
                        --      raise MyExceptions.Calc_Exception with "Multiplication overflow.";
                        --  end if;

                        NumOut := Num1 * Num2;
                        PushNumber(C, NumOut);

                elsif Operator = "/" then
                    -- check divide 0
                        if (Num2 = 0) then
                            Put_Line("Calc_Exception: Divide 0.");
                        end if;

                        -- check division overflow
                        Temp_R := Long_Long_Integer(num1) / Long_Long_Integer(num2);
                        if (Temp_R > Long_Long_Integer(Max_Integer) or 
                            Temp_R < Long_Long_Integer(Min_Integer)) then
                            Put_Line("Calc_Exception: Divition overflow.");
                        end if;

                        -- check division overflow
                        --  if (Num1 = Max_Integer and Num2 = -1) then
                        --      raise MyExceptions.Calc_Exception with "Division overflow.";
                        --  end if;

                        NumOut := Num1 / Num2;
                        PushNumber(C, NumOut);

                end if;
            -- If calc exception is raised, then push the number back
--              exception
--                  when MyExceptions.Calc_Exception =>
--                      PushNumber(C, Num2);
--                      PushNumber(C, Num1);
--                      NumOut := 0;
--                      -- continuing throw up the exception
--                      raise;
            end;      
        end if;
    end PerformOperation;


    -- For a string var, the procedure loads the value stored 
    -- in variable var and pushes it onto the stack.
    procedure LoadVar(C : in out MyCalculator; VarString: in String; Var : out VariableStore.Variable) is
    begin
        -- check lock state
        if (C.isLocked = True) then
            Put_Line("Lock_Exception: Calculator is locked.");
        else
            -- check whether the string is valid
            if not IsValidVarName(VarString) then
                Put_Line("Var_Exception: Variable name is invalid.");
            end if;

            declare
                V : VariableStore.Variable;
                Num : Item;
            begin
                -- check whether the variable exists
                V := VariableStore.From_String(VarString);
                if not VariableStore.Has_Variable(C.VariableDB, V) then
                    Put_Line("Var_Exception: Variable does not exist.");
                else
                    -- If exist, push the value onto the stack
                    Num := VariableStore.Get(C.VariableDB, V);
                    Var := V;
                    PushNumber(C, Num);
                end if;
            end;
            
        end if;
    end LoadVar;

    -- pops the value from the top of the stack and stores it 
    -- into variable var, defining that variable if it is not already defined.
    procedure StoreVar(C : in out MyCalculator; VarString: in String; Var : out VariableStore.Variable) is
    begin
        -- check lock state
        if (C.isLocked = True) then
            Put_Line("Lock_Exception: Calculator is locked.");
        else
            -- check whether the string is valid
            if not IsValidVarName(VarString) then
                Put_Line("Var_Exception: Variable name is invalid.");
            end if;

            declare
                V : VariableStore.Variable;
                Num : Item;
            begin
                -- pop the value from the top of the stack
                PopNumber(C, Num);

                -- store the value into variable var
                V := VariableStore.From_String(VarString);
                Var := V;
                VariableStore.Put(C.VariableDB, V, Num);
            end;
        end if;
    end StoreVar;

    -- makes variable var undefined (i.e. it will not be printed by subsequent “list” commands).
    procedure RemoveVar(C : in out MyCalculator; VarString: String; Var : out VariableStore.Variable) is 
    begin
        -- check lock state
        if (C.isLocked = True) then
            Put_Line("Lock_Exception: Calculator is locked.");
        else
            -- check whether the string is valid
            if not IsValidVarName(VarString) then
                Put_Line("Var_Exception: Variable name is invalid.");
            end if;

            declare
                V : VariableStore.Variable;
            begin
                -- check whether the variable exists
                V := VariableStore.From_String(VarString);
                Var := V;
                if not VariableStore.Has_Variable(C.VariableDB, V) then
                    Put_Line("Var_Exception: Variable does not exist.");
                else
                    -- remove the variable
                    VariableStore.Remove(C.VariableDB, V);
                end if;
            end;
        end if;
    end RemoveVar;

    -- prints out all currently defined variables and their corresponding values.
    procedure List(C : in MyCalculator) is
    begin
        -- check lock state
        if (C.isLocked = True) then
            Put_Line("Lock_Exception: Calculator is locked.");
        else
            -- print out all currently defined variables and their corresponding values
            VariableStore.Print(C.VariableDB);
        end if;
    end List;

end MyCalculator;
