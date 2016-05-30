with Analytical_Engine.Framework;

package body Analytical_Engine.Card_Reader is

   procedure Reset (This : out Instance)
   is
   begin
      This.Index := 1;
      This.Chain.Clear;
   end Reset;

   procedure Add_Cards (This : in out Instance; From_File_Named : String)
   is
      F : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Open (F,
                        Name => From_File_Named,
                        Mode => Ada.Text_IO.In_File);
      This.Add_Cards (F);
      Ada.Text_IO.Close (F);
   exception
      when Ada.Text_IO.End_Error =>
         Ada.Text_IO.Close (F);
   end Add_Cards;

   procedure Add_Cards (This : in out Instance; From : Ada.Text_IO.File_Type)
   is
   begin
      loop
         declare
            Line : constant String := Ada.Text_IO.Get_Line (From);
         begin
            This.Chain.Append (Card.Read (Line));
            --  exit when Ada.Text_IO.End_Of_File (From);
         end;
      end loop;
   exception
      when Ada.Text_IO.End_Error => null;
   end Add_Cards;

   procedure Execute (This             : in out Instance;
                      In_The_Framework : in out Framework.Instance)
   is
   begin
      This.Index := 1;
      This.Halted := False;
      loop
         declare
            C : constant Card.Card'Class := This.Chain (This.Index);
         begin
            This.Index := This.Index + 1;
            C.Execute (In_The_Framework);
         end;
         exit when This.Halted
           or else This.Index > Integer (This.Chain.Length);
      end loop;
      This.Halted := True;
   end Execute;

   procedure Step (This : in out Instance; By : Integer)
   is
   begin
      if not (This.Index + By in 1 .. Integer (This.Chain.Length)) then
         raise Card_Reader_Error with "step out of range";
      end if;
      This.Index := This.Index + By;
   end Step;

   procedure Halt (This : in out Instance)
   is
   begin
      This.Halted := True;
   end Halt;

   procedure Initialize (This : in out Instance)
   is
   begin
      This.Panel.Log_Attendant_Message ("card_reader initialized");
   end Initialize;

end Analytical_Engine.Card_Reader;
