package MyCommandLine with SPARK_Mode is

   function Argument_Count return Natural;

   function Command_Name return String;

   function Argument(Number : in Positive) return String with
     Pre => Number <= Argument_Count;

end MyCommandLine;
