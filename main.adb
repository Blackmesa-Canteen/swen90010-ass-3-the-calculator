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
with Ada.Characters.Latin_1;
with Ada.Strings.Fixed;

procedure Main is
   MAX_STACK_SIZE : constant Positive := 512;
   MAX_LINE_LENGTH : constant Positive := 2048;
   LOCKED_PREFIX : constant String := "locked> ";
   UNLOCKED_PREFIX : constant String := "unlocked> ";
   type Commands is (push, pop, load, store, remove, lock, unlock, list);
   package CC is new MyCalculator(MAX_STACK_SIZE);
   C : CC.MyCalculator;
   package Lines is new MyString(Max_MyString_Length => MAX_LINE_LENGTH + 1);
   S  : Lines.MyString;
begin

   -- check runtime arguments
   if ( MyCommandLine.Argument_Count /= 1 ) then
      Put_Line("Usage: "); 
      Put(MyCommandLine.Command_Name); 
      Put_Line(" <PIN>");
      return;
   end if;

   -- init the calculator with the PIN from the command line
   -- The PIN initially provided should always follow the given format
   if not CC.IsPin(MyCommandLine.Argument(1)) then
      Put_Line("PIN_Exception: PIN should be 0000 . . . 9999. ");
      return;
   end if;
   
   CC.Init(C, MyCommandLine.Argument(1));

   -- the main loop of the calculator
   loop
   declare
      -- Splitting the text into at most 3 tokens
      Tokens : MyStringTokeniser.TokenArray(1..3) := (others => (Start => 1, Length => 0));  
      SizeTokens : Natural;
      Command : Lines.MyString;
      Argument : Lines.MyString;
   begin
      -- print the prefix
      if CC.IsLocked(C) then
         Put(LOCKED_PREFIX);
      else
         Put(UNLOCKED_PREFIX);
      end if;

      -- read a line of input
      Lines.Get_Line(S);
      if Lines.Length(S) = 0 or Lines.To_String(S)'First >= Lines.To_String(S)'Last then
           Put_Line("Syntex_Exception: Do not provide empty input !");
           return;
      end if;
       
      
      for c of Lines.To_String(S) loop
          if c = Ada.Characters.Latin_1.NUL then
              Put_Line("Syntex_Exception: Do not include 'NUL' in your input!");
              return;
          end if;
      end loop;
      
      -- validate input: length
      if Lines.Length(S) > MAX_LINE_LENGTH then
        Put_Line("Syntex_Exception: Input too long!");
        return;
      end if;

      -- parse input into tokens array
      MyStringTokeniser.Tokenise(Lines.To_String(S),Tokens,SizeTokens);
      -- parse token
      -- check exceptional line
      if SizeTokens < 1 then
         Put_Line("Syntex_Exception: Empty entry!");
         return;
      elsif SizeTokens > 2 then
         Put_Line("Syntex_Exception: Too much arguments!");
         return;
      end if;

      -- parse commands and convert into string
      Command := Lines.Substring(S,Tokens(1).Start,Tokens(1).Start+Tokens(1).Length-1);
      declare
         CommandString : String := Lines.To_String(Command);
      
      begin     
      -- If the command is an operator
      if CC.IsValidOperator(CommandString) then
         -- check lock status
         if (CC.IsLocked(C)) then
            Put_Line("Lock_Exception: Calculator is locked!");
         else
            declare
               Result : Integer;
            begin
               if CC.Size(C) >= 2 then
                   CC.PerformOperation(C, CommandString, Result);    
               else
                   Put_Line("Stack_Exception: Require at least two numbers on stack to do calculation!");
               end if;
            end;
         end if;

      -- if the command is valid, but not an operator
      elsif CC.IsValidCommand(CommandString) then
         -- try to parse unary command
         if SizeTokens = 1 then
            -- check lock status
            if (CC.IsLocked(C)) then
               Put_Line("Lock_Exception: Calculator is locked!");
            else
               case Commands'Value(CommandString) is
                  -- pop and show the number
                  when pop =>
                     declare
                        NumOut : Integer;
                     begin
                     if CC.Size(C) <= 0 then
                        Put_Line("Stack_Exception: Stack is empty!");
                     else
                        CC.PopNumber (C, NumOut);
                        Put_Line(Integer'Image(NumOut));
                     end if;
                  end;
                  -- list the variable storage
                  when list =>
                     CC.List(C);
                  -- other undefined command
                  when others =>
                     Put_Line("Syntex_Exception: Unrecognized command!");
               end case;
            end if;

         -- try to parse binary command with its argument
         elsif SizeTokens = 2 then
            -- parse the argument
            Argument := Lines.Substring(S,Tokens(2).Start,Tokens(2).Start+Tokens(2).Length-1);
            declare
               ArgumentString : String := Lines.To_String(Argument);
            begin
            -- handle lock/unlock command logic
            if (CommandString = "lock" or CommandString = "unlock") then
               -- Check argument is the valid pin string or not
               if not CC.IsPin(ArgumentString) then
                  Put_Line("PIN_Exception: PIN should be 0000 . . . 9999. ");
               else
                  if (CommandString = "lock") then
                     -- if the calculator is already locked, raise exception
                     if (CC.IsLocked(C)) then
                        Put_Line("already locked!");
                     else
                        CC.Lock(C, ArgumentString);
                     end if;
                     
                  else
                     -- if the calculator is already unlocked, raise exception
                     if not CC.IsLocked(C) then
                        Put_Line("already unlocked!");
                     else
                        CC.UnLock(C, ArgumentString);
                     end if;
                  end if;
               end if;

            -- handles commands except lock/unlock logic
            else
               -- check lock status
               if (CC.IsLocked(C)) then
                  Put_Line("Lock_Exception: Calculator is locked!");
               else
                  case Commands'Value(CommandString) is
                     -- push the number
                     when push =>
                        declare
                           NumIn : Integer;
                        begin
                        -- convert string to integer
                        NumIn := StringToInteger.From_String(ArgumentString);
                        -- push the value
                        if CC.Size(C) > MAX_STACK_SIZE then
                           Put_Line("Stack_Exception: Stack is full!");
                        else
                           CC.PushNumber(C, NumIn);
                        end if;
                     end;

                     -- load the variable
                     when load =>
                        declare
                           VarOut : VariableStore.Variable;
                        begin
                        -- check the variable is valid or not
                        if not CC.IsValidVarName(ArgumentString) then
                           Put_Line("Var_Exception: Variable name is invalid.");
                        -- check the stack is full or not
                        elsif CC.Size(C) > MAX_STACK_SIZE then
                           Put_Line("Stack_Exception: Stack is full!");
                        else
                            CC.LoadVar(C, ArgumentString, VarOut);
                        end if;
                     end;
                        
                     -- store the variable
                     when store =>
                        declare
                           VarOut : VariableStore.Variable;
                        begin
                        -- check the variable is valid or not
                        if not CC.IsValidVarName(ArgumentString) then
                           Put_Line("Var_Exception: Variable name is invalid.");
                        -- check the stack is empty or not
                        elsif CC.Size(C) <= 0 then
                           Put_Line("Stack_Exception: Stack is empty!");
                        else
                           CC.StoreVar(C, ArgumentString, VarOut);
                        end if;
                        end;

                     -- remove the variable
                     when remove =>
                        declare
                           VarOut : VariableStore.Variable;
                        begin
                        -- check the variable is valid or not
                        if not CC.IsValidVarName(ArgumentString) then
                           Put_Line("Var_Exception: Variable name is invalid.");
                        else
                           CC.removeVar(C, ArgumentString, VarOut);
                        end if;
                        end;

                     -- other undefined command
                     when others =>
                        Put_Line("Syntex_Exception: Unrecognized command!");
                  end case;
               end if;
            end if;
            end;
         end if;
      else
         Put_Line("Syntex_Exception: Unrecognized command!");
      end if;
      end;
   
   end;
   end loop;

end Main;
