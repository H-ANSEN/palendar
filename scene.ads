package Scene is

    type Scene_I is interface;

    procedure OnLoad(self: out Scene_I) is abstract;
    procedure OnUnload(self: Scene_I) is abstract;
    procedure Draw(self: in out Scene_I) is abstract;
    procedure Update(self: in out Scene_I) is abstract;

end Scene;
