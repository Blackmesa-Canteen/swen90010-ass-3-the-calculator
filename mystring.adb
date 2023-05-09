with Ada.Text_IO;

package body MyString is

   function To_String(M : MyString) return String is
      Result : String(1..M.Length);
   begin
      Result := String(M.Str(M.Str'First..M.Length)) ;
      return Result;
   end To_String;

   function From_String(S : String) return MyString is
      M : MyString := (Length => 0, Str => (others => ' '));
      J : Integer := M.Str'First;
   begin
      if S'Length > Max_MyString_Length then
         raise Constraint_Error;
      end if;
      M.Length := S'Length;
      for I in S'Range loop
         pragma Loop_Invariant (J = I - S'First + 1);
         M.Str(J) := S(I);
         J := J + 1;
      end loop;
      return M;
   end From_String;

   function Less(M1 : MyString; M2 : MyString) return Boolean is
      I : Integer := M1.Str'First;
   begin
      If M1.Length < M2.Length then
         return True;
      elsif M1.Length > M2.Length then
         return False;
      else
         while (I <= M1.Str'Last) loop
            pragma Loop_Invariant (I >= M1.Str'First);
            if M1.Str(I) < M2.Str(I) then
               return True;
            elsif M1.Str(I) > M2.Str(I) then
               return False;
            else
               I := I + 1;
            end if;
         end loop;
         -- end of string and both equal
         return False;
      end if;
   end Less;


   function Equal(M1 : MyString; M2 : MyString) return Boolean is
      I : Integer := M1.Str'First;
   begin
      If M1.Length /= M2.Length then
         return False;
      else
         while (I <= M1.Str'Last) loop
            pragma Loop_Invariant (I >= M1.Str'First and
                                   (for all J in 1..I-1 => M1.Str(J) = M2.Str(J)));
            if M1.Str(I) /= M2.Str(I) then
               return False;
            else
               I := I + 1;
            end if;
         end loop;
         return True;
      end if;
   end Equal;

   function Substring(M : MyString; From : Positive; To : Positive) return MyString is
      R : MyString := (Length => To - From + 1, Str => (others => ' '));
      J : Positive := R.Str'First;
   begin
      for I in From..To loop
         pragma Loop_Invariant (J = I - From + 1);
         R.Str(J) := M.Str(I);
         J := J + 1;
      end loop;
      return R;
   end Substring;

   procedure Get_Line(M : out MyString) is
   begin
      Ada.Text_IO.Get_Line(Item => String(M.Str), Last => M.Length);
   end Get_Line;


end MyString;
