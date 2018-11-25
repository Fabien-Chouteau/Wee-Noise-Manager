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

with Gtk.Notebook;  use Gtk.Notebook;
with Gtk.Widget;    use Gtk.Widget;
with Gtk.Label;

with Gtk.Enums;

package body GUI.Track_Sequence is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self  : out Widget;
                      Track : Track_Id)
   is
   begin
      Self := new Widget_Record;
      Track_Sequence.Initialize (Self, Track);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self  : not null access Widget_Record'Class;
                         Track : Track_Id)
   is
   begin
      Initialize (Parent (Self));

      Self.Track := Track;

      Self.Set_Tab_Pos (Gtk.Enums.Pos_Left);
      Self.Set_Hexpand (True);
      Self.Set_Vexpand (True);

      GUI.Track_Pattern.Gtk_New (Self.Pattern, Track);
      Self.Append_Page (Self.Pattern, Gtk.Label.Gtk_Label_New ("Pattern"));

      GUI.Track_Params.Gtk_New (Self.Params, Track);
      Self.Append_Page (Self.Params, Gtk.Label.Gtk_Label_New ("Params"));
   end Initialize;

   ------------
   -- Update --
   ------------

   overriding
   procedure Update (Self : in out Widget_Record) is
   begin
      Self.Params.Update;
      Self.Pattern.Update;
   end Update;

   -----------------
   -- Reconfigure --
   -----------------

   overriding
   procedure Reconfigure (Self : in out Widget_Record) is
   begin
      Self.Params.Reconfigure;
   end Reconfigure;

end GUI.Track_Sequence;
