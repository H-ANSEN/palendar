with Raylib; use Raylib;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;

package Clok is

    function second_hand_coords(second : Second_Number; 
                                centerX, centerY, radius: Integer) return Vector2;

    function minute_hand_coords(minute: Minute_Number; 
                                second: Second_Number; 
                                centerX, centerY, radius: Integer) return Vector2;

    function hour_hand_coords(hour: Hour_Number;
                              minute: Minute_Number;
                              second: Second_Number;
                              centerX, centerY, radius: Integer) return Vector2;


end Clok;

