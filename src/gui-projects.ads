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

with Gtk.Box;

with Project_Manager; use Project_Manager;

private with GUI.Track;

package GUI.Projects is

   subtype Parent_Record is Gtk.Box.Gtk_Box_Record;
   subtype Parent is Gtk.Box.Gtk_Box;

   type Widget_Record is new Parent_Record and Updatable_Record with private;
   type Widget is access all Widget_Record'Class;

   procedure Gtk_New (Self  : out Widget);

   procedure Initialize (Self  : not null access Widget_Record'Class);

   overriding
   procedure Update (Self : in out Widget_Record);

private

   type Track_Array is array (Track_Id) of GUI.Track.Widget;

   type Widget_Record is new Parent_Record and Updatable_Record with record
      Tracks : Track_Array;
   end record;

end GUI.Projects;
