package PIN with SPARK_Mode is

   type PIN is private;

   function From_String(S : in String) return PIN with
     Pre => (S' Length = 4 and
               (for all I in S'Range => S(I) >= '0' and S(I) <= '9'));


private
   type PIN is new Natural range 0..9999;

end PIN;
