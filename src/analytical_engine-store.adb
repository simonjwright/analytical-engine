with Ada.Strings.Fixed;

package body Analytical_Engine.Store is

   procedure Set (This : in out Instance; Col : Column; To : Big_Integer)
   is
   begin
      if This.Panel.Tracing then
         This.Panel.Log_Trace_Message
           ("Store: "
              & Image (To)
              & " => V"
              & Ada.Strings.Fixed.Trim (Col'Img, Ada.Strings.Both));
      end if;
      Set (This.Columns (Col), To => To);
   end Set;

   procedure Get (This : Instance; Col : Column; Result : out Big_Integer)
   is
   begin
      if This.Panel.Tracing then
         This.Panel.Log_Trace_Message
           ("Store: V"
              & Ada.Strings.Fixed.Trim (Col'Img, Ada.Strings.Both)
              & "("
              & Image (This.Columns (Col))
              & ") => Mill");
      end if;
      Set (Result, To => This.Columns (Col));
   end Get;

   procedure Initialize (This : in out Instance)
   is
   begin
      This.Panel.Log_Attendant_Message ("store initialized");
   end Initialize;

end Analytical_Engine.Store;
