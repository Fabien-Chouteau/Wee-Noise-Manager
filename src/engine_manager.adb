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

with Sample_Manager;  use Sample_Manager;
with Project_Manager; use Project_Manager;

package body Engine_Manager is

   Engine_For_Track : array (Track_Id) of Track_Engine_Kind :=
     (1 => Sample,
      2 => Synth,
      3 => Effect,
      4 => Midi,
      5 => Input,
      others => LFO);

   Sub_For_Track : array (Track_Id) of Track_Sub_Engine_Id := (others => 1);

   -------------------
   -- Change_Engine --
   -------------------

   procedure Change_Engine (Track  : Project_Manager.Track_Id;
                            Engine : Track_Engine_Kind)
   is
   begin
      Engine_For_Track (Track) := Engine;
   end Change_Engine;

   --------------------
   -- Current_Engine --
   --------------------

   function Current_Engine (Track : Project_Manager.Track_Id)
                            return Track_Engine_Kind
   is (Engine_For_Track (Track));

   ----------------
   -- Change_Sub --
   ----------------

   procedure Change_Sub (Track  : Project_Manager.Track_Id;
                         Sub    : Track_Sub_Engine_Id)
   is
   begin
      Sub_For_Track (Track) := Sub;
   end Change_Sub;

   -----------------
   -- Current_Sub --
   -----------------

   function Current_Sub (Track  : Project_Manager.Track_Id)
                         return Track_Sub_Engine_Id
   is (Sub_For_Track (Track));

   -----------
   -- Valid --
   -----------

   function Valid  (Engine : Track_Engine_Kind;
                    Sub    : Track_Sub_Engine_Id)
                    return Boolean
   is
   begin
      case Engine is
         when Midi =>
            return Sub <= 15;
         when others =>
            return True;
      end case;
   end Valid;

   --------------
   -- Sub_Name --
   --------------

   function Sub_Name (Engine : Track_Engine_Kind;
                      Sub    : Track_Sub_Engine_Id)
                      return String
   is
   begin
      case Engine is
         when Sample =>
            return Sample_Manager.Name (Sample_Id (Sub));
         when Synth  =>
            return "Synth:" & Sub'Img;
         when Effect =>
            return "Effect" & Sub'Img;
         when Midi   =>
            return "Channel" & Sub'Img;
         when Input  =>
            return "Input" & Sub'Img;
         when LFO    =>
            return "LFO" & Sub'Img;
      end case;
   end Sub_Name;

   -----------
   -- Valid --
   -----------

   function Valid  (Engine : Track_Engine_Kind;
                    Sub    : Track_Sub_Engine_Id;
                    Param  : Project_Manager.Param_Id)
                    return Boolean
   is
      pragma Unreferenced (Sub);
   begin
      case Engine is
         when Sample => return Param < 3;
         when Synth  => return True;
         when Effect => return True;
         when Midi   => return True;
         when Input  => return Param < 2;
         when LFO    => return Param < 3;
      end case;
   end Valid;

   ----------------
   -- Param_Kind --
   ----------------

   function Param_Kind (Engine : Track_Engine_Kind;
                        Sub    : Track_Sub_Engine_Id;
                        Param  : Project_Manager.Param_Id)
                        return Parameter_Kind
   is
      pragma Unreferenced (Sub);
   begin
      case Engine is
         when Sample | Synth =>
            case Param is
               when 0      => return Volume;
               when 1      => return Pan;
               when 2      => return Pitch;
               when others => return None;
            end case;
         when Effect =>
            return Value;
         when Midi =>
            return Value;
         when Input =>
            case Param is
               when 0      => return Volume;
               when 1      => return Pan;
               when others => return None;
            end case;
         when LFO =>
            return Value;
      end case;
   end Param_Kind;

   ----------------
   -- Param_Name --
   ----------------

   function Param_Name (Engine : Track_Engine_Kind;
                        Sub    : Track_Sub_Engine_Id;
                        Param  : Project_Manager.Param_Id)
                        return String
   is
   begin
      case Engine is
         when Sample | Synth =>
            case Param is
               when 0      => return "Volume";
               when 1      => return "Pan";
               when 2      => return "Pitch";
               when others => return "Something";
            end case;
         when Effect =>
            return "Something";
         when Midi   =>
            return "CC" & Param'Img;
         when Input  =>
            case Sub is
               when 0      => return "Volume";
               when 1      => return "Pan";
               when others => return "Something";
            end case;
         when LFO    =>
            return "Something";
      end case;
   end Param_Name;

end Engine_Manager;
