library(trackframe) #FIXME: remove once transferred to travelpaths
library(travelpaths)
set.seed(2025)
path_matrix <- sim_travel_path(size = 1000, format = "matrix")
save(path_matrix, file = "~/travelpaths-devel/pkgs/trackframe/data/path_matrix.rda", compress="xz")

set.seed(2025)
path_data_frame <- sim_travel_path(size = 1000, format = "data.frame")
save(path_data_frame, file = "~/travelpaths-devel/pkgs/trackframe/data/path_data_frame.rda", compress="xz")

set.seed(2025)
path_trackframe <- sim_travel_path(size = 1000, format = "trackframe")
save(path_trackframe, file = "~/travelpaths-devel/pkgs/trackframe/data/path_trackframe.rda", compress="xz")

set.seed(2025)
path_sftrack <- sim_travel_path(size = 1000, format = "sftrack")
save(path_sftrack, file = "~/travelpaths-devel/pkgs/trackframe/data/path_sftrack.rda", compress="xz")

set.seed(2025)
path_move2 <- sim_travel_path(size = 1000, format = "move2")
save(path_move2, file = "~/travelpaths-devel/pkgs/trackframe/data/path_move2.rda", compress="xz")


#simulate_travel_paths
set.seed(2025)
paths_matrix <- sim_travel_paths(ntracks = 3, sizes = 1000, format = "matrix")
save(paths_matrix, file = "~/travelpaths-devel/pkgs/trackframe/data/paths_matrix.rda", compress="xz")

set.seed(2025)
paths_data_frame <- sim_travel_paths(ntracks = 3, sizes = 1000, format = "data.frame")
save(paths_data_frame, file = "~/travelpaths-devel/pkgs/trackframe/data/paths_data_frame.rda", compress="xz")

set.seed(2025)
paths_trackframe <- sim_travel_paths(ntracks = 3, sizes = 1000, format = "trackframe")
save(paths_trackframe, file = "~/travelpaths-devel/pkgs/trackframe/data/paths_trackframe.rda", compress="xz")

set.seed(2025)
paths_sftrack <- sim_travel_paths(ntracks = 3, sizes = 1000, format = "sftrack")
save(paths_sftrack, file = "~/travelpaths-devel/pkgs/trackframe/data/paths_sftrack.rda", compress="xz")

set.seed(2025)
paths_move2 <- sim_travel_paths(ntracks = 3, sizes = 1000, format = "move2")
save(paths_move2, file = "~/travelpaths-devel/pkgs/trackframe/data/paths_move2.rda", compress="xz")

set.seed(2025)
tf_mini <- sim_travel_paths(3, c(5, 4, 2))
save(tf_mini, file = "~/travelpaths-devel/pkgs/trackframe/data/tf_mini.rda", compress="xz")

set.seed(2025)
df_mini <- sim_travel_path(5, format = "data.frame")
save(df_mini, file = "~/travelpaths-devel/pkgs/trackframe/data/df_mini.rda", compress="xz")

set.seed(2025)
sftrack_mini <- sim_travel_path(5, format = "sftrack")
save(sftrack_mini, file = "~/travelpaths-devel/pkgs/trackframe/data/sftrack_mini.rda", compress="xz")

set.seed(2025)
move2_mini <- sim_travel_path(5, format = "move2")
save(move2_mini, file = "~/travelpaths-devel/pkgs/trackframe/data/move2_mini.rda", compress="xz")
