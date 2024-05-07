with Ada.Calendar; use Ada.Calendar;

with Raylib; use Raylib;
with RGUI.Component; use RGUI.Component;

package Clock is

    type Clock_T is new Component_T with record
        ntime    : Time    := Ada.Calendar.Clock;
    end record;

    overriding procedure Draw(self: in out Clock_T);
    overriding procedure Update(self: in out Clock_T);

end Clock;

