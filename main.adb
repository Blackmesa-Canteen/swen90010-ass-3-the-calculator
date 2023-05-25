-- Author
-- Xiaotian Li, Student ID: 1141181
-- Junrong Wu, Student ID: 1310531

-- The Security Property that we have been proved for this project is as follows:

-- 1. The arithmetic operations ("+", "-", "*", ""), load, store, remove, and lock operations can 
-- only ever be performed when the calculator is in the unlocked state. 

-- In our implementation, all the arithmetic operations were taken care of and encapsulated with in the 
-- procedure of PerformOperation() (specifications of it can be found on line 69 of mycalculator.ads). We did the security check simply
-- by including a precondition in the procedure specification of that method -> "Pre => isLocked(C) = False", which
-- implies a precondition of before performing any arithmetic operations (calling that procedure), this precondition
-- should always be fulfilled first. After we running this precondition through the SPARK prover, no complaint was made
-- by the SPARK prover, thus, we can believe that this security property has been satisfied. Meanwhile, before actually
-- calling this method in main.adb, additional check has been performed checking whether the calculator is currently locked
-- or not, if it is locked, exception would be shown through a print out manner indicating it as an invalid operation to do now.
-- Finally, before the procedure was actually called, assertion was placed before it to make sure that the calculator is
-- actually unlocked before performing calculations.

-- 2. The Unlock operation can only ever be performed when the calculator is in the locked state and vice versa.

-- This security property was also proved through a manner of putting preconditions before the Unlock() and Lock() procedure specified in the mycalculator.ads
-- No complain was made by the SPARK prover indicating the security property to be true. 
-- To strength the prove, we've also put judgements before these two methods before they are actually called, if the calculator was
-- already locked/unlocked when calling Lock()/Unlock() method, error message would be printed out in the terminal stopping the user
-- from approaching to attempt it again.

-- 3. The Lock operation, when it is performed, should update the master PIN with the new PIN that is supplied.

-- This security property was proved through a manner of putting postcondition of PIN."="(PinIn, GetPin(C)) after the Lock() procedure
-- is performed specified in the mycalculator.ads. Same as the two previous properties, no complain was made by the SPARK prover
-- indicating that this security property to be true. To strength the prove, assertion of pragma Assert(CC.IsPin(ArgumentString) = True) was made
-- before updating the PIN in the system making sure that the provided update PIN is a valid PIN.

-- 4. ADDITIONAL: When the program is started, the provided command line of the initial PIN for the calculator should not be empty, including NUL or not following the PIN format.

-- This security property might be a property which is out-of-scope here, however it is an important property that might often be ignored.
-- This property was not directly proved through the SPARK prover since it does not have the capability to do so, however, if an invalid, NUL included or empty
-- PIN was provided when the program first starts, it would be a severe problem harming the further run of the system. This property was guranteed
-- by adding pre-checks on the command line arguments before the program runs, it can be found at the very beginning section of the main.adb file,
-- whenever an invalid PIN was provided initially, the program would be refusing to execute, and returning the correct use to the user through the command line argument.

-- 5. ADDITIONAL: User Input should not be empty, full of spaces, including 'NUL' characters, end with spaces or exceeding the maximum length

-- The string tokeniser is taken in place to deal with the user input in the system, thus the user input should be strictly
-- checked and make sure its a valid one that can be used by the system. For the empty input and 'NUL' character included inputs, it was directly picked up by the 
-- SPARK prover automatically with counterexamples of: 1. input'First >= input'Last 2. input'First = 0, input'Last = 4 (others => 'NUL'). While for the input full of
-- spaces and end with spaces, it was found by manual testing after SPARK has rised a concern on the input format as we just mentioned, SPARK has provided us with 
-- a counter example that after string tokenising, the token length was actually shrinked by 1 or directly shown to be zero it might be due to the implementation of the string tokeniser, 
-- that it could not handle inputs with more than one spaces included, since it seperates tokens with spaces. Finally, for the exceeding maximum length, it was according to the specification
-- of the assignment with a maximum input length limited. This property was proved by putting pre-checks before the user input is actually used by the main.adb and starts
-- tokenising, if either of these scenarios takes in place, the system would consider it as an invalid input and stop the program from further processing.

-- 6. ADDITIONAL: Overall correctness of the stack. When pushing a number to the stack (performing the "push" operator), the stack should not be full, the pushed number should sit on the top of the stack, 
-- other elements within the stack should remain unchanged and the size of the stack should be increased.

-- For this security property, it was obtained from the common properties and understanding of a stack and any array like elements in programming. It was proved through
-- the preconditions and postconditions specified in the mycalculator.ads on the PushNumber() procedure. SPARK prover has no complain on these conditions, meaning that this property is correct and supported.

-- 7. ADDITIONAL: Overall correctness of the stack. When popping a number from the stack (performing the "pop" operator), the stack should not be empty, the popped number should be popped from the top of the stack, 
-- other elements within the stack should remain unchanged and the size of the stack should be decreased.

-- For this security property, it was obtained from the common properties and understanding of a stack and any array like elements in programming. It was proved through
-- the preconditions and postconditions specified in the mycalculator.ads on the PopNumber() procedure. SPARK prover has no complain on these conditions, meaning that this property is correct and supported.

-- 8. ADDITIONAL: When overflow takes in place during arithmetic operation, the stack should remain unchanged

-- For this security property, it was automatically picked up by the SPARK prover through counter examples indicates that overflows might be taken in place during calculations. Therefore, we've performed judgement
-- in the implementation around line 130 to line 160 in mycalculator.adb, when overflow takes in place, the system would return an error from the terminal indicating the issue. Meanwhile, to strengthen
-- the prove, on line 74 of mycalculator.ads postcondition of (Size(C) = Size(C'Old)) is provided indicating that some of the times the stack size should remain unchanged. This postcondition was not
-- complained by the SPARK prover, thus we believe that this security property is supported by our implementation.

-- 9. ADDITIONAL: When performing any arithmetic operation, there should be at least two elements already on the stack currently 

-- For this security property, it was proved on line 71 with a precondition of Size(C) >= 2, SPARK prover did not complain about this precondition and thus this property can be proved. Meanwhile, to strengthen the
-- prove, before performing any operations, in main.adb, we've placed judgement on it to check the stack size, if the user attempt to do such a behaviour, the system would stop them and return an error message printed
-- out in the terminal.

-- 10. ADDITIONAL: When performing "load" and "store" operation, the variable name should be a valid one.

-- For this security property, it was checked by the postcondition in line 78 and 90 in mycalculator.ads (commands can be seen there). SPARK prover did not complain about these postconditions, thus we believe
-- that this security property is proved and supported by our implementation.

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
use Ada.Strings.Fixed;


procedure Main is
   MAX_STACK_SIZE : constant Positive := 512;
   MAX_LINE_LENGTH : constant Positive := 2048;
   LOCKED_PREFIX : constant String := "locked> ";
   UNLOCKED_PREFIX : constant String := "unlocked> ";
   package CC is new MyCalculator(MAX_STACK_SIZE);
   C : CC.MyCalculator;
   package Lines is new MyString(Max_MyString_Length => MAX_LINE_LENGTH + 1);
   S  : Lines.MyString;
   Tokens : MyStringTokeniser.TokenArray(1..3) := (others => (Start => 1, Length => 0));  
   SizeTokens : Natural;
   Command : Lines.MyString;
   Argument : Lines.MyString;
   Space_Count: Integer;
   VarDb: VariableStore.Database;
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

   VariableStore.Init(VarDb);
   CC.Init(C, MyCommandLine.Argument(1));

   -- the main loop of the calculator
   loop
   -- pragma Loop_Invariant (CC.GetVarDb(C) = VarDb);
   begin
      -- print the prefix
      if CC.IsLocked(C) then
         Put(LOCKED_PREFIX);
      else
         Put(UNLOCKED_PREFIX);
      end if;

      -- read a line of input
      Lines.Get_Line(S);
      
      -- Check whether user input is empty 
      if Lines.Length(S) = 0 or Lines.To_String(S)'First > Lines.To_String(S)'Last then
           Put_Line("Syntex_Exception: Do not provide empty input !");
           return;
      end if;

      Space_Count := 0;
      -- Check whether user input are full of spaces
      for c of Lines.To_String(S) loop
          if c = ' ' and Space_Count < Integer'Last then
              Space_Count := Space_Count + 1;
          end if;
       end loop;
         
      -- Check whether user input are full of spaces
      if Space_Count = Lines.Length(S) then
          Put_Line("Syntex_Exception: Do not provide input with all spaces!");
          return;
      end if;
      if (Index_Non_Blank(Lines.To_String(S)) = 0) then
         Put_Line("Syntex_Exception: Do not provide empty input !");
         return;
      end if;
         
      -- Check whether user input includes 'NUL'   
      for c of Lines.To_String(S) loop
          if c = Ada.Characters.Latin_1.NUL then
              Put_Line("Syntex_Exception: Do not include 'NUL' in your input!");
              return;
          end if;
      end loop;
      
      -- Check whether user input ends with a SPACE to make sure it can be correctly utilised by the string tokeniser   
      if Lines.To_String(S)(Lines.To_String(S)'Last) = ' ' then
          Put_Line("Syntex_Exception: Please do not end your input with a SPACE!");
          return;
      end if;
         
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
      pragma Assert(Tokens(1).Start+Tokens(1).Length-1 <= Lines.Length(S));
      Command := Lines.Substring(S,Tokens(1).Start,Tokens(1).Start+Tokens(1).Length-1);
      begin   
      -- If the command is an operator
      if CC.IsValidOperator(Lines.To_String(Command)) then
         -- check lock status
         if (CC.IsLocked(C)) then
            Put_Line("Lock_Exception: Calculator is locked!");
         else
            pragma Assert(CC.IsLocked(C) = False);
            if CC.Size(C) >= 2 then
               CC.PerformOperation(C, Lines.To_String(Command));    
            else
               Put_Line("Stack_Exception: Require at least two numbers on stack to do calculation!");
            end if;
         end if;

      -- if the command is valid, but not an operator
      elsif CC.IsValidCommand(Lines.To_String(Command)) then
         -- try to parse unary command
         if SizeTokens = 1 then
            -- check lock status
            if (CC.IsLocked(C)) then
               Put_Line("Lock_Exception: Calculator is locked!");
            else
               pragma Assert(CC.IsLocked(C) = False);
               if Lines.To_String(Command) = "pop" then
                  -- pop and show the number
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
               elsif Lines.To_String(Command) = "list" then
                  -- list the variable storage
                  VariableStore.Print(VarDb);
               else
                  -- other undefined command
                  Put_Line("Syntex_Exception: Unrecognized command!");
               end if;
            end if;

         -- try to parse binary command with its argument
         elsif SizeTokens = 2 then
            -- parse the argument
            pragma Assert(Tokens(2).Start+Tokens(2).Length-1 <= Lines.Length(S));
            Argument := Lines.Substring(S,Tokens(2).Start,Tokens(2).Start+Tokens(2).Length-1);
            declare
                ArgumentString: String := Lines.To_String(Argument);
            begin
            -- handle lock/unlock command logic
            if (Lines.To_String(Command) = "lock" or Lines.To_String(Command) = "unlock") then
               -- Check argument is the valid pin string or not
               if not CC.IsPin(ArgumentString) then
                  Put_Line("PIN_Exception: PIN should be 0000 . . . 9999. ");
               else
                  if (Lines.To_String(Command) = "lock") then
                     -- if the calculator is already locked, raise exception
                     if (CC.IsLocked(C)) then
                        Put_Line("already locked!");
                     else
                        pragma Assert(CC.IsPin(ArgumentString) = True);
                        CC.Lock(C, PIN.From_String(ArgumentString));
                     end if;
                     
                  else
                     -- if the calculator is already unlocked, raise exception
                     if not CC.IsLocked(C) then
                        Put_Line("already unlocked!");
                     else
                        pragma Assert(CC.IsPin(ArgumentString) = True);
                        CC.Unlock(C, PIN.From_String(ArgumentString));
                     end if;
                  end if;
               end if;

            -- handles commands except lock/unlock logic
            else
               -- check lock status
               if (CC.IsLocked(C)) then
                  Put_Line("Lock_Exception: Calculator is locked!");
               else
                  pragma Assert(CC.IsLocked(C) = False);
                  if Lines.To_String(Command) = "push" then
                     -- push the val
                     declare
                           NumIn : Integer;
                        begin
                        -- convert string to integer
                        NumIn := StringToInteger.From_String(ArgumentString);
                        -- push the value
                        if CC.Size(C) >= MAX_STACK_SIZE then
                           Put_Line("Stack_Exception: Stack is full!");
                        else
                           CC.PushNumber(C, NumIn);
                        end if;
                     end;

                  elsif Lines.To_String(Command) = "load" then
                     -- load the variable
                     declare
                           Var : VariableStore.Variable;
                        begin
                        -- check the variable is valid or not
                        if not CC.IsValidVarName(ArgumentString) then
                           Put_Line("Var_Exception: Variable name is invalid.");
                        -- check the stack is full or not
                        elsif CC.Size(C) >= MAX_STACK_SIZE then
                           Put_Line("Stack_Exception: Stack is full!");
                        else
                           Var := VariableStore.From_String(ArgumentString);
                           if not VariableStore.Has_Variable(VarDb, Var) then
                              Put_Line("Var_Exception: Variable is not found.");
                           else
                              pragma Assert (VariableStore.Has_Variable(VarDb, Var));
                              CC.LoadVar(C, VarDb, Var); 
                           end if;
                        end if;
                     end;

                  elsif Lines.To_String(Command) = "store" then
                     -- store the variable
                     declare
                           Var : VariableStore.Variable;
                        begin
                        -- check the variable is valid or not
                        if not CC.IsValidVarName(ArgumentString) then
                           Put_Line("Var_Exception: Variable name is invalid.");
                        else
                           -- check the stack is empty or not
                           if CC.Size(C) <= 0 then
                              Put_Line("Stack_Exception: Stack is empty!");
                           else
                              Var := VariableStore.From_String(ArgumentString);
                              pragma Assert (CC.IsValidVarName(ArgumentString));
                              CC.StoreVar(C, VarDb, Var);
                           end if;
                        end if;
                     end;

                  elsif Lines.To_String(Command) = "remove" then
                     -- remove the variable
                     declare
                           Var : VariableStore.Variable;
                        begin
                        -- check the variable is valid or not
                        if not CC.IsValidVarName(ArgumentString) then
                           Put_Line("Var_Exception: Variable name is invalid.");
                        else
                           Var := VariableStore.From_String(ArgumentString);
                           pragma Assert (CC.IsValidVarName(ArgumentString));
                           CC.removeVar(C, VarDb, Var);
                        end if;
                     end;
                  else
                     -- other undefined command
                     Put_Line("Syntex_Exception: Unrecognized command!");
                  end if;
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
