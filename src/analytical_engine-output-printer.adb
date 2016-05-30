with Ada.Text_IO; use Ada.Text_IO;
package body Analytical_Engine.Output.Printer is

   procedure Initialize (This : in out Instance)
   is
      pragma Unreferenced (This);
   begin
      Put_Line ("output.printer initialized");
   end Initialize;

   procedure Output (To : Instance; S : String)
   is
      pragma Unreferenced (To);
   begin
      Put_Line (S);
   end Output;

end Analytical_Engine.Output.Printer;
