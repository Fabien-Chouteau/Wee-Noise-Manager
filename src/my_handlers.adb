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

with Ada.Text_IO;
with Gtk.Enums, Gtk.Box;
use  Gtk.Enums, Gtk.Box;

with Gtk.Event_Box; use Gtk.Event_Box;

with Sample_Manager; use Sample_Manager;
with GUI.Resource_Usage;

package body My_Handlers is

   use type Sample_Manager.Sample_Id;

   package Memory_Usage_Pck is new GUI.Resource_Usage (Sample_Id);

   ---------
   -- Arf --
   ---------

   procedure Arf (P : access Gtk_Css_Provider_Record'Class) is
      pragma Unreferenced (P);
   begin
      Ada.Text_IO.Put_Line ("css error");
   end Arf;

end My_Handlers;
