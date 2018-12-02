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

with Gtk.Widget; use Gtk.Widget;

package body GUI is

   -----------------------
   -- Widget_From_Child --
   -----------------------

   function Widget_From_Child
     (W  : access Gtk_Widget_Record'Class)
      return not null access Widget_Record'Class
   is
      Parent : Gtk_Widget := W.Get_Parent;
   begin
      while Parent.all not in Widget_Record'Class loop
         Parent := Parent.Get_Parent;

         if Parent = null then
            raise Program_Error with "Cannot find parent";
         end if;
      end loop;

      return Widget (Parent);
   end Widget_From_Child;

end GUI;
