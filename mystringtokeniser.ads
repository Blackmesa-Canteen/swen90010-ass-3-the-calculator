with Ada.Characters.Latin_1;

package MyStringTokeniser with SPARK_Mode is

   -- Define token structure (set of the start index and the end index)
   type TokenExtent is record
      Start : Positive;
      Length : Natural;
   end record;

   -- Define array of tokens
   type TokenArray is array(Positive range <>) of TokenExtent;

   -- Check whether a char is a whitespace or not, regardless of the locale
   function Is_Whitespace(Ch : Character) return Boolean is
     (Ch = ' ' or Ch = Ada.Characters.Latin_1.LF or
        Ch = Ada.Characters.Latin_1.HT);

   -- Tokenise a string into an array of tokens
   procedure Tokenise(S : in String; Tokens : in out TokenArray; Count : out Natural) with
     -- Precondition: If S is not null, then tokens is not null
     Pre => (if S'Length > 0 then S'First <= S'Last) and Tokens'First <= Tokens'Last,
     -- Postcondition: Count number is not greater than the size of Tokens array, and
     -- for each counted token in Tokens array, the start index is within the range of S, and
     -- if the length of the token is greater than 0, then the end index is within the range of S.
     -- Using 'and then' to prevent condition check exception when the length of the token is 0.
     --
     -- The postcondition guarantees that all counted tokens are not exceeding the String S's index bound.
     Post => Count <= Tokens'Length and
     (for all Index in Tokens'First..Tokens'First+(Count-1) =>
          (Tokens(Index).Start >= S'First and
          Tokens(Index).Length > 0) and then
            Tokens(Index).Length-1 <= S'Last - Tokens(Index).Start);


end MyStringTokeniser;
