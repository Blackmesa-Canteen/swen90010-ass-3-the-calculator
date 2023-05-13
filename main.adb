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

procedure Main is
   MAX_STACK_SIZE : constant Positive := 512;
   MAX_LINE_LENGTH : constant Positive := 2048;
   LOCKED_PREFIX : constant String := "locked> ";
   UNLOCKED_PREFIX : constant String := "unlocked> ";
   package MyCalculator is new MyCalculator(MAX_STACK_SIZE, Integer, 0);
   C : MyCalculator.Calculator;
   package Lines is new MyString(Max_MyString_Length => MAX_LINE_LENGTH + 1);
   S  : Lines.MyString;
begin

   -- check runtime arguments
   if ( MyCommandLine.Argument_Count /= 1 ) then
      Put_Line("Usage: "); 
      Put(MyCommandLine.Command_Name); 
      Put_Line(" <PIN>");
      exit;
   end if;

   -- init the calculator with the PIN from the command line
   MyCalculator.Init(C, MyCommandLine.Argument(1));

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
      if MyCalculator.IsLocked(C) then
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
      Command := Lines.To_String(
         Lines.Substring(S,Tokens(1).Start,Tokens(1).Start+Tokens(1).Length-1));
      CommandString := MyString.To_String(Command);
      
      -- If the command is an operator
      if MyCalculator.IsValidOperator(CommandString) then
         -- check lock status
         if (MyCalculator.IsLocked(C)) then
            raise MyExceptions.Lock_Exception with "Calculator is locked!";
         else
            MyCalculator.PerformOperation(CommandString);
         end if;

      -- if the command is valid, but not an operator
      elsif MyCalculator.IsValidCommand(CommandString) then
         -- try to parse unary command
         if SizeTokens = 1 then
            -- check lock status
            if (MyCalculator.IsLocked(C)) then
               raise MyExceptions.Lock_Exception with "Calculator is locked!";
            else
               case CommandString is
                  -- pop and show the number
                  when "pop" =>
                     declare
                        NumOut : Integer;
                     begin
                     if MyCalculator.Size(C) <= 0 then
                        raise MyExceptions.Stack_Exception with "Stack is empty!";
                     else
                        MyCalculator.PopNumber (C, NumOut);
                        Put_Line(Integer'Image(NumOut));
                     end if;
                     end;
                  -- list the variable storage
                  when  "list" =>
                     MyCalculator.ListVariables(C);

                  -- other undefined command
                  when others =>
                     raise MyExceptions.Syntex_Exception with "Unrecognized command!";
               end case;
            end if;

         -- try to parse binary command with its argument
         elsif SizeTokens = 2 then
            -- parse the argument
            Argument := Lines.To_String(
               Lines.Substring(S,Tokens(2).Start,Tokens(2).Start+Tokens(2).Length-1));
            ArgumentString := MyString.To_String(Argument);

            -- handle lock/unlock command logic
            if (CommandString = "lock" or CommandString = "unlock") then
               -- Check argument is the valid pin string or not
               if not MyCalculator.IsPin(ArgumentString) then
                  raise MyExceptions.PIN_Exception with "PIN should be 0000 . . . 9999. ";
               else
                  if (CommandString = "lock") then
                     -- if the calculator is already locked, raise exception
                     if (MyCalculator.IsLocked(C)) then
                        raise MyExceptions.Lock_Exception with "already locked!";
                     else
                        MyCalculator.Lock(C, ArgumentString);
                     end if;
                     
                  else
                     -- if the calculator is already unlocked, raise exception
                     if not MyCalculator.IsLocked(C) then
                        raise MyExceptions.Lock_Exception with "already unlocked!";
                     else
                        MyCalculator.UnLock(C, ArgumentString);
                     end if;
                  end if;
               end if;

            -- handles commands except lock/unlock logic
            else
               -- check lock status
               if (MyCalculator.IsLocked(C)) then
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
                        if MyCalculator.Size(C) > MAX_STACK_SIZE then
                           raise MyExceptions.Stack_Exception with "Stack is full!";
                        else
                           MyCalculator.PushNumber(C, NumIn);
                        end if;
                        end;

                     -- load the variable
                     when "load" =>
                        declare
                           VarOut : VariableStore.Variable;
                        begin
                        -- check the variable is valid or not
                        if not MyCalculator.IsValidVarName(ArgumentString) then
                           raise MyExceptions.Var_Exception with "Variable name is invalid.";
                        else
                            MyCalculator.LoadVar(C, ArgumentString, VarOut);
                        end if;
                        end;
                        
                     -- store the variable
                     when "store" =>
                        declare
                           VarOut : VariableStore.Variable;
                        begin
                        -- check the variable is valid or not
                        if not MyCalculator.IsValidVarName(ArgumentString) then
                           raise MyExceptions.Var_Exception with "Variable name is invalid.";
                        else
                           MyCalculator.StoreVar(C, ArgumentString, VarOut);
                        end if;
                        end;

                     -- remove the variable
                     when "remove" =>
                        declare
                           VarOut : VariableStore.Variable;
                        begin
                        -- check the variable is valid or not
                        if not MyCalculator.IsValidVarName(ArgumentString) then
                           raise MyExceptions.Var_Exception with "Variable name is invalid.";
                        else
                           MyCalculator.removeVar(C, ArgumentString, VarOut);
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
         exit;
   end;
   end loop;

end Main;

procedure Backup_Main is
   DB : VariableStore.Database;
   V1 : VariableStore.Variable := VariableStore.From_String("Var1");
   PIN1  : PIN.PIN := PIN.From_String("1234");
   PIN2  : PIN.PIN := PIN.From_String("1234");
   package Lines is new MyString(Max_MyString_Length => 2048);
   S  : Lines.MyString;
begin

   Put(MyCommandLine.Command_Name); Put_Line(" is running!");
   Put("I was invoked with "); Put(MyCommandLine.Argument_Count,0); Put_Line(" arguments.");
   for Arg in 1..MyCommandLine.Argument_Count loop
      Put("Argument "); Put(Arg,0); Put(": """);
      Put(MyCommandLine.Argument(Arg)); Put_Line("""");
   end loop;

   VariableStore.Init(DB);
   Put_Line("Adding an entry to the database");
   VariableStore.Put(DB,V1,10);

   Put_Line("Reading the entry:");
   Put(VariableStore.Get(DB,V1));
   New_Line;
   
   Put_Line("Printing out the database: ");
   VariableStore.Print(DB);
   
   Put_Line("Removing the entry");
   VariableStore.Remove(DB,V1);
   If VariableStore.Has_Variable(DB,V1) then
      Put_Line("Entry still present! It is: ");
      Put(VariableStore.Get(DB,V1));
      New_Line;
   else
      Put_Line("Entry successfully removed");
   end if;

   Put_Line("Reading a line of input. Enter some text (at most 3 tokens): ");
   Lines.Get_Line(S);

   Put_Line("Splitting the text into at most 5 tokens");
   declare
      T : MyStringTokeniser.TokenArray(1..5) := (others => (Start => 1, Length => 0));
      NumTokens : Natural;
   begin
      MyStringTokeniser.Tokenise(Lines.To_String(S),T,NumTokens);
      Put("You entered "); Put(NumTokens); Put_Line(" tokens.");
      for I in 1..NumTokens loop
         declare
            TokStr : String := Lines.To_String(Lines.Substring(S,T(I).Start,T(I).Start+T(I).Length-1));
         begin
            Put("Token "); Put(I); Put(" is: """);
            Put(TokStr); Put_Line("""");
         end;
      end loop;
      if NumTokens > 3 then
         Put_Line("You entered too many tokens --- I said at most 3");
      end if;
   end;

   If PIN."="(PIN1,PIN2) then
      Put_Line("The two PINs are equal, as expected.");
   end if;
   
   declare
      Smallest_Integer : Integer := StringToInteger.From_String("-2147483648");
      R : Long_Long_Integer := 
        Long_Long_Integer(Smallest_Integer) * Long_Long_Integer(Smallest_Integer);
   begin
      Put_Line("This is -(2 ** 32) (where ** is exponentiation) :");
      Put(Smallest_Integer); New_Line;
      
      if R < Long_Long_Integer(Integer'First) or
         R > Long_Long_Integer(Integer'Last) then
         Put_Line("Overflow would occur when trying to compute the square of this number");
      end if;
         
   end;
   Put_Line("2 ** 32 is too big to fit into an Integer...");
   Put_Line("Hence when trying to parse it from a string, it is treated as 0:");
   Put(StringToInteger.From_String("2147483648")); New_Line;    
end Backup_Main;
