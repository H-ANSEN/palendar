with Raylib; use Raylib;
with RGUI.Component; use RGUI.Component;

package Notepad is

    type Notepad_T is new Component_T with record
        fnt      : Font  := GetFontDefault;
        fnt_size : Float := 30.0;
    end record;

    overriding procedure Draw(self: in out Notepad_T);
    overriding procedure Update(self: in out Notepad_T);

private 

    LINE_FEED : constant Integer := 10;

end Notepad;
