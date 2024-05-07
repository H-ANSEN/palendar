
package RGUI.Component.Rec is

    type Rec_T is new Component_T with record
        null;
    end record;

    overriding procedure Draw(self: in out Rec_T);
    overriding procedure Update(self: in out Rec_T);

end RGUI.Component.Rec;
