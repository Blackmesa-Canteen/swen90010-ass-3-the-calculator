with Ada.Containers.Formal_Ordered_Maps;
with Ada.Containers;
use Ada.Containers;

with MyString;

package VariableStore with SPARK_Mode is
   Max_Variable_Length : constant Natural := 1024;
   Max_Entries : constant Ada.Containers.Count_Type := 1000;

   package Variables is new MyString(Max_MyString_Length => Max_Variable_Length);

   type Variable is new Variables.MyString;

   type Database is private;

   procedure Init(D : out Database);

   function Has_Variable(D : in Database; V : in Variable) return Boolean;

   function Get(D : in Database; V : in Variable) return Integer with
     Pre => Has_Variable(D,V);

   procedure Put(D : in out Database; V : in Variable; Value : in Integer) with
     Pre => Length(D) < Max_Entries or Has_Variable(D,V);

   procedure Remove(D : in out Database; V : in Variable) with
     Pre => Has_Variable(D,V);

   function From_String(S : String) return Variable with
     Pre => S'Length <= Max_Variable_Length;
   function To_String(V : Variable) return String;

   function Less(V1 : Variable; V2 : Variable) return Boolean;
   function Equal(V1 : Variable; V2 : Variable) return Boolean;

   function Length(D : in Database) return Ada.Containers.Count_Type;
   
   procedure Print(D : in Database);
   
private
   pragma SPARK_Mode(Off);

   package PW_Ordered_Maps is new
     Ada.Containers.Formal_Ordered_Maps
       (Key_Type        => Variable,
        Element_Type    => Integer,
        "<" => Less);


   type Database is record
      variables : PW_Ordered_Maps.Map(Capacity => Max_Entries);
   end record;


   function Get(D : in Database; V : in Variable) return Integer is
     (PW_Ordered_Maps.Element(Container => D.variables,Key => V));
   function Has_Variable(D : in Database; V : in Variable) return Boolean is
     (PW_Ordered_Maps.Contains(Container => D.variables, Key => V));

   function From_String(S : String) return Variable is
     (Variable(Variables.From_String(S)));
   function To_String(V : Variable) return String is
     (Variables.To_String(Variables.MyString(V)));

   function Less(V1 : Variable; V2 : Variable) return Boolean is
      (Variables.Less(Variables.MyString(V1),Variables.MyString(V2)));
   function Equal(V1 : Variable; V2 : Variable) return Boolean is
      (Variables.Equal(Variables.MyString(V1),Variables.MyString(V2)));

   function Length(D : in Database) return Ada.Containers.Count_Type is
      (PW_Ordered_Maps.Length(D.variables));
end VariableStore;
