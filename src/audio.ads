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

with Interfaces;

package Audio is

   type Mono_Frame is new Interfaces.Integer_16
     with Size => 16, Object_Size => 16;

   type Stereo_Frame is record
      L, R : Mono_Frame;
   end record
     with Size => 32, Object_Size => 32;

   type Mono_Buffer is array (Natural range <>) of Mono_Frame
     with Pack;
   type Stereo_Buffer is array (Natural range <>) of Stereo_Frame
     with Pack;

end Audio;
