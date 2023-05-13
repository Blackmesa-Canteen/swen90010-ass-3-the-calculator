pragma SPARK_Mode (On);

with StringToInteger;
with VariableStore;
with MyCommandLine;
with MyString;
with MyStringTokeniser;
with PIN;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with MyExceptions;
with MyCalculator;
with Ada.Exceptions;  use Ada.Exceptions;
with VariableStore; use VariableStore;

with Ada.Long_Long_Integer_Text_IO;
with Ada.Command_Line;

procedure Main is
   MAX_STACK_SIZE : constant Positive := 512;
   MAX_LINE_LENGTH : constant Positive := 2048;
   LOCKED_PREFIX : constant String := "locked> ";
   UNLOCKED_PREFIX : constant String := "unlocked> ";
   package CC is new MyCalculator(MAX_STACK_SIZE, Integer, 0);
   C : CC.MyCalculator;
   package Lines is new MyString(Max_MyString_Length => MAX_LINE_LENGTH + 1);
   S  : Lines.MyString;
begin

   -- check runtime arguments
   if ( MyCommandLine.Argument_Count /= 1 ) then
      Put_Line("Usage: "); 
      Put(MyCommandLine.Command_Name); 
      Put_Line(" <PIN>");
      Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Failure);
   end if;

   -- init the calculator with the PIN from the command line
   CC.Init(C, MyCommandLine.Argument(1));

   -- the main loop of the calculator
   loop
   declare
      -- Splitting the text into at most 3 tokens: [prefix, command, argument]
      Tokens : MyStringTokeniser.TokenArray(1..3) := (others => (Start => 1, Length => 0));
      SizeTokens : Natural;
      Command : Lines.MyString := Lines.From_String("");
      CommandString : String := "";
      Argument : Lines.MyString := Lines.From_String("");
      ArgumentString : String := "";
   begin
      -- print the prefix
      if CC.IsLocked(C) then
         Put(LOCKED_PREFIX);
      else
         Put(UNLOCKED_PREFIX);
      end if;

      -- read a line of input
      Lines.Get_Line(S);

      -- validate input: length
      if Lines.Length(S) > MAX_LINE_LENGTH then
         raise MyExceptions.Syntex_Exception with "Input too long!";
      end if;

      -- parse input into tokens array
      MyStringTokeniser.Tokenise(Lines.To_String(S),Tokens,SizeTokens);

      -- parse token
      -- check exceptional line
      if SizeTokens < 1 then
         raise MyExceptions.Syntex_Exception with "Empty entry!";
      elsif SizeTokens > 2 then
         raise MyExceptions.Syntex_Exception with "Too much arguments!";
      end if;

      -- parse commands and convert into string
      Command := Lines.Substring(S,Tokens(1).Start,Tokens(1).Start+Tokens(1).Length-1);
      CommandString := Lines.To_String(Command);
      
      -- If the command is an operator
      if CC.IsValidOperator(CommandString) then
         -- check lock status
         if (CC.IsLocked(C)) then
            raise MyExceptions.Lock_Exception with "Calculator is locked!";
         else
            declare
               Result : Integer;
            begin
            CC.PerformOperation(C, CommandString, Result);
            end;
         end if;

      -- if the command is valid, but not an operator
      elsif CC.IsValidCommand(CommandString) then
         -- try to parse unary command
         if SizeTokens = 1 then
            -- check lock status
            if (CC.IsLocked(C)) then
               raise MyExceptions.Lock_Exception with "Calculator is locked!";
            else
               case CommandString is
                  -- pop and show the number
                  when "pop" =>
                     declare
                        NumOut : Integer;
                     begin
                     if CC.Size(C) <= 0 then
                        raise MyExceptions.Stack_Exception with "Stack is empty!";
                     else
                        CC.PopNumber (C, NumOut);
                        Put_Line(Integer'Image(NumOut));
                     end if;
                     end;
                  -- list the variable storage
                  when  "list" =>
                     CC.ListVariables(C);

                  -- other undefined command
                  when others =>
                     raise MyExceptions.Syntex_Exception with "Unrecognized command!";
               end case;
            end if;

         -- try to parse binary command with its argument
         elsif SizeTokens = 2 then
            -- parse the argument
            Argument := Lines.Substring(S,Tokens(2).Start,Tokens(2).Start+Tokens(2).Length-1);
            ArgumentString := Lines.To_String(Argument);

            -- handle lock/unlock command logic
            if (CommandString = "lock" or CommandString = "unlock") then
               -- Check argument is the valid pin string or not
               if not CC.IsPin(ArgumentString) then
                  raise MyExceptions.PIN_Exception with "PIN should be 0000 . . . 9999. ";
               else
                  if (CommandString = "lock") then
                     -- if the calculator is already locked, raise exception
                     if (CC.IsLocked(C)) then
                        raise MyExceptions.Lock_Exception with "already locked!";
                     else
                        CC.Lock(C, ArgumentString);
                     end if;
                     
                  else
                     -- if the calculator is already unlocked, raise exception
                     if not CC.IsLocked(C) then
                        raise MyExceptions.Lock_Exception with "already unlocked!";
                     else
                        CC.UnLock(C, ArgumentString);
                     end if;
                  end if;
               end if;

            -- handles commands except lock/unlock logic
            else
               -- check lock status
               if (CC.IsLocked(C)) then
                  raise MyExceptions.Lock_Exception with "Calculator is locked!";
               else
                  case CommandString is
                     -- push the number
                     when "push" =>
                        declare
                           NumIn : Integer;
                        begin
                        -- convert string to integer
                        NumIn := StringToInteger.From_String(ArgumentString);
                        -- push the value
                        if CC.Size(C) > MAX_STACK_SIZE then
                           raise MyExceptions.Stack_Exception with "Stack is full!";
                        else
                           CC.PushNumber(C, NumIn);
                        end if;
                        end;

                     -- load the variable
                     when "load" =>
                        declare
                           VarOut : VariableStore.Variable;
                        begin
                        -- check the variable is valid or not
                        if not CC.IsValidVarName(ArgumentString) then
                           raise MyExceptions.Var_Exception with "Variable name is invalid.";
                        else
                            CC.LoadVar(C, ArgumentString, VarOut);
                        end if;
                        end;
                        
                     -- store the variable
                     when "store" =>
                        declare
                           VarOut : VariableStore.Variable;
                        begin
                        -- check the variable is valid or not
                        if not CC.IsValidVarName(ArgumentString) then
                           raise MyExceptions.Var_Exception with "Variable name is invalid.";
                        else
                           CC.StoreVar(C, ArgumentString, VarOut);
                        end if;
                        end;

                     -- remove the variable
                     when "remove" =>
                        declare
                           VarOut : VariableStore.Variable;
                        begin
                        -- check the variable is valid or not
                        if not CC.IsValidVarName(ArgumentString) then
                           raise MyExceptions.Var_Exception with "Variable name is invalid.";
                        else
                           CC.removeVar(C, ArgumentString, VarOut);
                        end if;
                        end;

                     -- other undefined command
                     when others =>
                        raise MyExceptions.Syntex_Exception with "Unrecognized command!";
                  end case;
               end if;
            end if;
         end if;
      else
         raise MyExceptions.Syntex_Exception with "Unrecognized command!";
      end if;
   
   -- deal with exceptions in loop
   exception
      -- deal with non-major exceptions without exit system
      when E : MyExceptions.Lock_Exception =>
         Put_Line(Exception_Message (E));
      when E : MyExceptions.PIN_Exception =>
         Put_Line(Exception_Message (E));
      when E : MyExceptions.Operator_Exception =>
         Put_Line(Exception_Message (E));
      when E : MyExceptions.Calc_Exception =>
         Put_Line(Exception_Message (E));
      when E : MyExceptions.Var_Exception =>
         Put_Line(Exception_Message (E));

      -- deal with major exceptions with exiting system
      when E : others =>
         Put_Line(Exception_Message (E));
         Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Failure);
   end;
   end loop;

end Main;
