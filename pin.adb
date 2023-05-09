package body PIN with SPARK_Mode is

   function From_String(S : in String) return PIN is
      Result : Natural := 0;
      Count : Natural := 0;
   begin
      for I in S'Range loop
         declare
            Ch : Character := S(I);
         begin
            --pragma Assert (Character'Pos(Ch) - Character'Pos('0') <= 9);
            pragma Loop_Invariant (Ch >= '0' and Ch <= '9' and Count = I-S'First and Count <= 4 and  Result < 10 ** Count);
            Count := Count + 1;
            Result := Result * 10;
            Result := Result + (Character'Pos(Ch) - Character'Pos('0'));
         end;
      end loop;
      return PIN(Result);
   end From_String;



end PIN;
