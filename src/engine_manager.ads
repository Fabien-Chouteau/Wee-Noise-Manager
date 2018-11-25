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

with Sample_Manager;
with Project_Manager;

package Engine_Manager is

   type Track_Engine_Kind is (Sample, Synth, Effect, Midi, Input, LFO);
   --  Main categories of sound engines

   type Track_Sub_Engine_Id is new Sample_Manager.Sample_Id;
   --  Sub level of sound engine.
   --
   --  For instance the Sub id of the sample engine is the sample id

   type Parameter_Kind is (None, Volume, Pan, Pitch, Value);

   procedure Change_Engine (Track  : Project_Manager.Track_Id;
                            Engine : Track_Engine_Kind);

   function Current_Engine (Track : Project_Manager.Track_Id)
                            return Track_Engine_Kind;

   procedure Change_Sub (Track  : Project_Manager.Track_Id;
                         Sub    : Track_Sub_Engine_Id);

   function Current_Sub (Track  : Project_Manager.Track_Id)
                         return Track_Sub_Engine_Id;

   function Valid  (Engine : Track_Engine_Kind;
                    Sub    : Track_Sub_Engine_Id)
                    return Boolean;
   --  Return True if Sub is a valid id for this engine

   function Sub_Name (Engine : Track_Engine_Kind;
                      Sub    : Track_Sub_Engine_Id)
                      return String;
   --  Return a printable name for the Sub of Engine

   function Valid  (Engine : Track_Engine_Kind;
                    Sub    : Track_Sub_Engine_Id;
                    Param  : Project_Manager.Param_Id)
                    return Boolean;
   --  Return True if Param is a valid parameter id for Sub of Engine

   function Param_Kind (Engine : Track_Engine_Kind;
                        Sub    : Track_Sub_Engine_Id;
                        Param  : Project_Manager.Param_Id)
                        return Parameter_Kind;

   function Param_Name (Engine : Track_Engine_Kind;
                        Sub    : Track_Sub_Engine_Id;
                        Param  : Project_Manager.Param_Id)
                        return String;
   --  Return a printable name for Param of Sub of Engine

   function Img (Kind : Track_Engine_Kind) return String
   is (case Kind is
          when Sample => "Sample",
          when Synth  => "Synth",
          when Effect => "Effect",
          when Midi   => "Midi",
          when Input  => "Input",
          when LFO    => "LFO");

end Engine_Manager;
