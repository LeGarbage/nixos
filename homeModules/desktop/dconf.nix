# Generated via dconf2nix: https://github.com/gvolpe/dconf2nix
{ lib, ... }:

with lib.hm.gvariant;

{
  dconf.settings = {
    "io/github/alainm23/planify" = {
      appearance = "Dark";
      automatic-reminders-enabled = true;
      calendar-enabled = true;
      dark-mode = true;
      home-view = "scheduled";
      labels-show-active-only = false;
      pane-position = 300;
      run-in-background = true;
      start-week = "Sunday";
      system-appearance = true;
      task-complete-tone = false;
      views-order-visible = [
        "today"
        "scheduled"
        "all-items-view"
        "completed"
        "labels"
        "unlabeled-view"
      ];
    };

    "io/missioncenter/MissionCenter" = {
      apps-page-sorting-order = "Ascending";
      first-time-running = false;
    };

    "org/gnome/calendar" = {
      active-view = "month";
    };

    "org/gtk/gtk4/settings/file-chooser" = {
      show-hidden = true;
      sort-directories-first = true;
    };

    "org/gtk/settings/file-chooser" = {
      date-format = "regular";
      location-mode = "path-bar";
      show-hidden = true;
      show-size-column = true;
      show-type-column = true;
      sidebar-width = 212;
      sort-column = "name";
      sort-directories-first = false;
      sort-order = "ascending";
      type-format = "category";
    };
  };
}
