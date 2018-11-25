-------------------------------------------------------------------------------
--                                                                           --
--                             Wee Noise Manager                             --
--                                                                           --
--                     Copyright (C) 2018 Fabien Chouteau                    --
--                                                                           --
--    Wee Noise Manager is free software: you can redistribute it and/or     --
--    modify it under the terms of the GNU General Public License as         --
--    published by the Free Software Foundation, either version 3 of the     --
--    License, or (at your option) any later version.                        --
--                                                                           --
--    Wee Noise Manager is distributed in the hope that it will be useful,   --
--    but WITHOUT ANY WARRANTY; without even the implied warranty of         --
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU       --
--    General Public License for more details.                               --
--                                                                           --
--    You should have received a copy of the GNU General Public License      --
--    along with Wee Noise Manager.                                          --
--    If not, see <http://www.gnu.org/licenses/>.                            --
--                                                                           --
-------------------------------------------------------------------------------

with Gtk.Toggle_Button; use Gtk.Toggle_Button;
with Gtk.Widget;    use Gtk.Widget;

package body GUI.Step_Enabled is

   procedure On_Toggled (W : access Gtk_Toggle_Button_Record'Class);

   ----------------
   -- On_Toggled --
   ----------------

   procedure On_Toggled (W : access Gtk_Toggle_Button_Record'Class) is
      Self : constant Widget := Widget (W);
   begin
      Self.Val := Self.Get_Active;
   end On_Toggled;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Widget;
                      Track : Track_Id;
                      Step  : Step_Id)
   is
   begin
      Self := new Widget_Record;
      Step_Enabled.Initialize (Self, Track, Step);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self  : not null access Widget_Record'Class;
                         Track : Track_Id;
                         Step  : Step_Id)
   is
   begin
      Initialize (Parent (Self));

      Self.Track := Track;
      Self.Step  := Step;
      Self.Val   := False;

      Self.On_Toggled (On_Toggled'Access);
   end Initialize;

   -----------
   -- Value --
   -----------

   function Value (Self : Widget_Record) return Boolean
   is (Self.Val);

   ------------
   -- Update --
   ------------

   procedure Update (Self : in out Widget_Record) is
   begin
      raise Program_Error;
   end Update;

end GUI.Step_Enabled;
