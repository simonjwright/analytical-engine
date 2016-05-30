package body Analytical_Engine.Attendant is

   procedure Initialize (This : in out Instance)
   is
   begin
      This.Panel.Log_Attendant_Message ("attendant initialized");
   end Initialize;

end Analytical_Engine.Attendant;
