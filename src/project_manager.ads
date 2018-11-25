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

package Project_Manager is

   type Sequence_Id is range 0 .. 15;
   type Track_Id is range 0 .. 15;
   type Step_Id is range 0 .. 15;
   type Param_Id is range 0 .. 9;

   type Param_Value is range 0 .. 127;

   type Step_Condition_Kind is (P100, P25, P50, P75);

   type Track_Engine_Kind is (Sample, Synth, Effect, Midi, Input, LFO);

   type Track_Sub_Engine_Id is range 0 .. 99;

   function Img (Kind : Step_Condition_Kind) return String
   is (case Kind is
          when P100 => "100%",
          when P25  => "25%",
          when P50  => "50%",
          when P75  => "75%");

   function Img (Kind : Track_Engine_Kind) return String
   is (case Kind is
          when Sample => "Sample",
          when Synth  => "Synth",
          when Effect => "Effect",
          when Midi   => "Midi",
          when Input  => "Input",
          when LFO    => "LFO");

end Project_Manager;
