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

with Gtk.Notebook; use Gtk.Notebook;
with Gtk.Widget;    use Gtk.Widget;
with Gtk.Enums;

package body GUI.Track_Params is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self  : out Widget;
                      Track : Track_Id)
   is
   begin
      Self := new Widget_Record;
      Track_Params.Initialize (Self, Track);
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

      Self.Set_Tab_Pos (Gtk.Enums.Pos_Bottom);

      for Param in Param_Id loop
         Gtk.Label.Gtk_New (Self.Labels (Param), Param'Img);

         GUI.Params_Page.Gtk_New (Self.Pages (Param), Track, Param);

         Self.Append_Page (Self.Pages (Param),
                           Self.Labels (Param));
      end loop;

   end Initialize;
   ------------
   -- Update --
   ------------

   overriding
   procedure Update (Self : in out Widget_Record) is
   begin
      raise Program_Error;
   end Update;

end GUI.Track_Params;
