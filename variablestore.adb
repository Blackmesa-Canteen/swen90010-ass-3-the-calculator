with Ada.Text_IO;
with Ada.Integer_Text_IO;

package body VariableStore Is

   procedure Init(D : out Database) is
   begin
      PW_Ordered_Maps.Clear(D.variables);
   end Init;

   procedure Put(D : in out Database; V : in Variable; Value : in Integer) is
   begin
      PW_Ordered_Maps.Include(D.variables,V,Value);
   end Put;

   procedure Remove(D : in out Database; V : in Variable) is
   begin
      PW_Ordered_Maps.Delete(D.variables,V);
   end Remove;
   
   procedure Print(D : in Database) is
      C : PW_Ordered_Maps.Cursor := PW_Ordered_Maps.No_Element;
      V : Variable;
      Value : Integer;
   begin
      C := PW_Ordered_Maps.First(D.Variables);
      while (PW_Ordered_Maps."/="(C, PW_Ordered_Maps.No_Element)) loop
         V := PW_Ordered_Maps.Key(D.Variables, C);
         Value := PW_Ordered_Maps.Element(D.Variables, C);
         Ada.Text_IO.Put("   ");
         Ada.Text_IO.Put(To_String(V));
         Ada.Text_IO.Put(" => ");
         Ada.Integer_Text_IO.Put(Value);
         Ada.Text_IO.New_Line;
         PW_Ordered_Maps.Next(D.Variables, C);         
      end loop;
   end Print;
   
end VariableStore;
