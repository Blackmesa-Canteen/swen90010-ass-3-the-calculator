
package body MyStringTokeniser with SPARK_Mode is



   procedure Tokenise(S : in String; Tokens : in out TokenArray; Count : out Natural) is
      Index : Positive;
      Extent : TokenExtent;
      OutIndex : Integer := Tokens'First;
   begin
      -- count for valid tokens
      Count := 0;
      -- ignore errornous string
      if (S'First > S'Last) then
         return;
      end if;

      -- Pointer to the first character of the string
      Index := S'First;

      -- traverse within the String
      while OutIndex <= Tokens'Last and Index <= S'Last and Count < Tokens'Length loop
         -- Loop_Invariant 1:
         -- Guarantee the correct state: During the loop, all tokens in the output Tokens array
         -- should be valid, i.e. the start index should be within the range of the input string,
         -- and if the length is greater than 0, the end index should also be within the range.
         pragma Loop_Invariant
           (for all J in Tokens'First..OutIndex-1 =>
              (Tokens(J).Start >= S'First and
                   Tokens(J).Length > 0) and then
            Tokens(J).Length-1 <= S'Last - Tokens(J).Start);

         -- Loop_Invariant 2:
         -- Guarantee the correct state: The output Tokens array's index should always be
         -- the initial value plus the count number of valid tokens, i.e. guarantee 
         -- OutIndex always point to the end of the output Token array. So that to avoid
         -- result overwrites and index overflow.
         pragma Loop_Invariant (OutIndex = Tokens'First + Count);

         -- look for start of next token
         while (Index >= S'First and Index < S'Last) and then Is_Whitespace(S(Index)) loop
            Index := Index + 1;
         end loop;
         if (Index >= S'First and Index <= S'Last) and then not Is_Whitespace(S(Index)) then
            -- found a token
            Extent.Start := Index;
            Extent.Length := 0;

            -- look for end of this token
            while Positive'Last - Extent.Length >= Index and then (Index+Extent.Length >= S'First and Index+Extent.Length <= S'Last) and then not Is_Whitespace(S(Index+Extent.Length)) loop
               Extent.Length := Extent.Length + 1;
            end loop;

            Tokens(OutIndex) := Extent;
            Count := Count + 1;

            -- check for last possible token, avoids overflow when incrementing OutIndex
            if (OutIndex = Tokens'Last) then
               return;
            else
               OutIndex := OutIndex + 1;
            end if;

            -- check for end of string, avoids overflow when incrementing Index
            if S'Last - Extent.Length < Index then
               return;
            else
               Index := Index + Extent.Length;
            end if;
         end if;
      end loop;
   end Tokenise;

end MyStringTokeniser;
