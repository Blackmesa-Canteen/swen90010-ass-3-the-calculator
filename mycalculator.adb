package body MyCalculator with SPARK_Mode is
    
    procedure MultiplyBothPositive(Num1: in Integer; Num2: in Integer; Result: out Integer) is
        begin
        Result := Num1 * Num2;
    end MultiplyBothPositive;
   
    procedure MultiplyBothNegative(Num1: in Integer; Num2: in Integer; Result: out Integer) is
        begin
        Result := Num1 * Num2;
    end MultiplyBothNegative;
    
    procedure MultiplyPositiveNegative(Num1: in Integer; Num2: in Integer; Result: out Integer) is
        begin
        Result := Num1 * Num2;
    end MultiplyPositiveNegative;
   
    -- Init the calc
    procedure Init(C : out MyCalculator; MasterPINString : in String) is
    begin
        -- init master pin
        C.MasterPIN := PIN.From_String(MasterPINString);
      
        -- point to stack bottom
        C.size := 0;
        -- init stack array
        C.storage := (others => 0);

        -- default is locked
        C.isLocked := True;

    end Init;

    -- try unlock. 
    procedure Unlock(C : in out MyCalculator; PinIn : PIN.PIN)is
    begin
        -- if locked, compare password and try to unlock
        if (PIN."="(PinIn, C.MasterPIN)) then
            C.isLocked := False;
        else
            -- wrong password
            Put_Line("Password is wrong.");
        end if;

    end Unlock;

    -- try lock
    procedure Lock(C : in out MyCalculator; PinIn : PIN.PIN) is
    begin
        -- set a new master pin, and then lock the calculator
        C.MasterPIN := PinIn;
        C.isLocked := True;
    end Lock;

    -- Push a number in
    procedure PushNumber(C : in out MyCalculator; NumIn : in Item) is
    begin
        C.size := C.size + 1;
        C.storage(C.size) := NumIn;
    end PushNumber;

    -- pop a number out
    procedure PopNumber(C : in out MyCalculator; NumOut : out Item) is
    begin
        NumOut := C.storage(C.size);
        C.size := C.size - 1;
    end PopNumber;

    -- The commands “+”, “-”, “*” and “/” each pop the top two values 
    -- from the operand stack and compute the corresponding arithmetic 
    -- operation on them (addition, subtraction, multiplication and division, respectively), 
    -- and push the result onto the stack.
    procedure PerformOperation(C : in out MyCalculator; 
                               Operator : in String) is
    begin
        -- pop the top two values from the operand stack
            declare 
                Num1 : Item;
                Num2 : Item;
                Result: Item;
                Max_Integer : constant Integer := Integer'Last;
                Min_Integer : constant Integer := Integer'First;
                Temp_R : Long_Long_Integer;
            begin                
                -- pop out the number
                pragma Assert (not IsLocked(C));
                PopNumber(C, Num1);
                pragma Assert (not IsLocked(C));
                PopNumber(C, Num2);
                if Num1 > Max_Integer or Num2 > Max_Integer or Num1 < Min_Integer or Num2 < Min_Integer then
                    Put_Line("hello");
                end if;
                 -- compute the corresponding arithmetic operation on them
                if Operator = "+" then
                     -- check addition overflow
                        Temp_R := Long_Long_Integer(Num1) + Long_Long_Integer(Num2);
                        if (Temp_R > Long_Long_Integer(Max_Integer) or 
                            Temp_R < Long_Long_Integer(Min_Integer)) then
                            -- rollback the stack, show error info
                            pragma Assert (not IsLocked(C));
                            PushNumber(C, Num2);
                            pragma Assert (not IsLocked(C));
                            PushNumber(C, Num1);
                            Put_Line("Addition overflow.");
                            return;
                        end if;

                        -- check addition positive overflow
                        --  if (IsNum2Possitive and Num1 > Max_Integer - Num2) then
                        --      raise MyExceptions.Calc_Exception with "Addition overflow.";
                        --  end if;

                        -- check addition negative overflow
                        --  if (not IsNum2Possitive and Num1 < Min_Integer - Num2) then
                        --      raise MyExceptions.Calc_Exception with "Addition overflow.";
                        --  end if;
                        pragma Assert (not IsLocked(C));
                        PushNumber(C, Num1 + Num2);
                elsif Operator = "-" then
                    -- check substraction overflow
                        Temp_R := Long_Long_Integer(Num1) - Long_Long_Integer(Num2);
                        if (Temp_R > Long_Long_Integer(Max_Integer) or 
                            Temp_R < Long_Long_Integer(Min_Integer)) then
                            -- rollback the stack, show error info
                            pragma Assert (not IsLocked(C));
                            PushNumber(C, Num2);
                            pragma Assert (not IsLocked(C));
                            PushNumber(C, Num1);
                            Put_Line("Substraction overflow.");
                            return;
                        end if;

                        -- check subtraction positive overflow
                        --  if (not IsNum2Possitive and Num1 > Max_Integer + Num2) then
                        --      raise MyExceptions.Calc_Exception with "Subtraction overflow.";
                        --  end if;

                        -- check substraction negative overflow
                        --  if (IsNum2Possitive and Num1 < Min_Integer + Num2) then
                        --      raise MyExceptions.Calc_Exception with "Subtraction overflow.";
                        --  end if;
                        pragma Assert (not IsLocked(C));
                        PushNumber(C, Num1 - Num2);
                elsif Operator = "*" then
                        -- check multiplication overflow
                        declare
                            IsProductZero : Boolean := (Num1 = 0 or Num2 = 0);
                            IsProductPositive : Boolean := (Num1 > 0 and Num2 > 0) or (Num1 < 0 and Num2 < 0);
                            IsProductNegative : Boolean := (Num1 > 0 and Num2 < 0) or (Num1 < 0 and Num2 > 0);
                        begin
                            if (IsProductZero) then
                                pragma Assert (not IsLocked(C));
                                PushNumber(C, 0);
                            elsif (IsProductPositive) then
                                if ((Num1 > 0 and Num2 > 0) and then Num1 <= Max_Integer / Num2) then
                                    pragma Assert (not IsLocked(C));
                                    MultiplyBothPositive(Num1, Num2, Result);
                                    PushNumber(C, Result);
                                elsif ((Num1 < 0 and Num2 < 0) and then (Num1 /= Min_Integer and Num2 /= Min_Integer) and then (-Num1) <= Max_Integer / (-Num2)) then
                                    pragma Assert (not IsLocked(C));
                                    MultiplyBothNegative(Num1, Num2, Result);
                                    PushNumber(C, Result);
                                else
                                    -- rollback the stack, show error info
                                    pragma Assert (not IsLocked(C));
                                    PushNumber(C, Num2);
                                    pragma Assert (not IsLocked(C));
                                    PushNumber(C, Num1);
                                    Put_Line("Multiplication overflow.");

                                end if;  
                            elsif (IsProductNegative) then
                                if ((Num1 >= (Min_Integer + 1) / Num2) and (Num1 < 0 and Num2 > 0)) 
                                or ((Num2 >= (Min_Integer + 1) / Num1) and (Num1 > 0 and Num2 < 0)) then
                                    pragma Assert (not IsLocked(C));
                                    MultiplyPositiveNegative(Num1,Num2,Result);
                                    PushNumber(C, Result);
                                else

                                    -- rollback the stack, show error info
                                    pragma Assert (not IsLocked(C));
                                    PushNumber(C, Num2);
                                    pragma Assert (not IsLocked(C));
                                    PushNumber(C, Num1);
                                    Put_Line("Multiplication overflow.");

                                end if;
                            end if;
                        end;
                elsif Operator = "/" then
                    -- check divide 0
                        if (Num2 = 0) then
                            -- rollback the stack, show error info
                            pragma Assert (not IsLocked(C));
                            PushNumber(C, Num2);
                            pragma Assert (not IsLocked(C));
                            PushNumber(C, Num1);
                            Put_Line("Divition 0.");
                            return;
                        end if;

                        -- check division overflow
                        Temp_R := Long_Long_Integer(Num1) / Long_Long_Integer(Num2);
                        if (Temp_R > Long_Long_Integer(Max_Integer) or 
                            Temp_R < Long_Long_Integer(Min_Integer)) then
                            -- rollback the stack, show error info
                            pragma Assert (not IsLocked(C));
                            PushNumber(C, Num2);
                            pragma Assert (not IsLocked(C));
                            PushNumber(C, Num1);
                            Put_Line("Divition overflow.");
                            return;
                        end if;

                        -- check division overflow
                        --  if (Num1 = Max_Integer and Num2 = -1) then
                        --      raise MyExceptions.Calc_Exception with "Division overflow.";
                        --  end if;
                        pragma Assert (not IsLocked(C));
                        PushNumber(C, Num1 / Num2);
                else
                        -- rollback the stack, show error info
                        pragma Assert (not IsLocked(C));
                        PushNumber(C, Num2);
                        pragma Assert (not IsLocked(C));
                        PushNumber(C, Num1);
                        Put_Line("Invalid operator.");
                end if;
            end;
    end PerformOperation;


    -- For a string var, the procedure loads the value stored 
    -- in variable var and pushes it onto the stack.
    procedure LoadVar(C : in out MyCalculator; VarDb : in VariableStore.Database; Var : in VariableStore.Variable) is
    begin
        declare
            Num : Item;
        begin
            if VariableStore.Has_Variable(VarDb, Var) then
                Num := VariableStore.Get(VarDb, Var);
                PushNumber(C, Num);
            else
                Put_Line("Variable is undefined in stack.");
            end if;
        end;
     
    end LoadVar;

    -- pops the value from the top of the stack and stores it 
    -- into variable var, defining that variable if it is not already defined.
    procedure StoreVar(C : in out MyCalculator; VarDb : in out VariableStore.Database; Var : in VariableStore.Variable) is
    begin
        declare
            Num : Item;
        begin
            if (VariableStore.Length(VarDb) < VariableStore.Max_Entries or VariableStore.Has_Variable(VarDb,Var)) then
                -- pop the value from the top of the stack
                PopNumber(C, Num);
                -- store the value into variable var
                VariableStore.Put(VarDb, Var, Num);
            else
                Put_Line("Varable store is full.");
            end if;
        end;
    
    end StoreVar;

    -- makes variable var undefined (i.e. it will not be printed by subsequent “list” commands).
    procedure RemoveVar(C : in MyCalculator; VarDb : in out VariableStore.Database; Var : in VariableStore.Variable) is 
    begin
        -- remove the variable
        if VariableStore.Has_Variable(VarDb, Var) then
            VariableStore.Remove(VarDb, Var);
        end if;
    end RemoveVar;
end MyCalculator;
