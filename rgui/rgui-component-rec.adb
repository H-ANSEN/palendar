
package body RGUI.Component.Rec is

    overriding procedure Draw(self: Rec_T) is 
    begin 
        DrawRectangleRec((self.x, self.y, self.width, self.height), self.bg_color);     
    end;

    overriding procedure Update(self: in out Rec_T) is begin null; end;

end RGUI.Component.Rec;
