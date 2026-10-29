library(euroleaguer)
library(dplyr)
library(tidyverse)

euroleague_history = euroleaguer::getCompetitionHistory("E")

euroleague_teams_25_26 = euroleaguer:: getCompetitionTeams("E2025")

euroleague_phases_25_26 = euroleaguer:: getCompetitionPhases("E2025")

euroleague_rounds_25_26 = euroleaguer:: getCompetitionRounds("E2025")

euroleague_round_one_25_26 = euroleaguer:: getCompetitionGames("E2025", 1, phase_type = "RS")


## Trial Game - Panathinaikos v. Bayern Round 1

bayern_pan_25_26 = euroleague_round_one_25_26 |> 
  filter(GameCode == 6) |> 
  select(SeasonCode, Round, PhaseType, Identifier, GameCode, HomeCode, HomeName, HomeScore, HomeQuartersQ1, HomeQuartersQ2, HomeQuartersQ3, 
         HomeQuartersQ4, HomeQuartersOt1, HomeQuartersOt2, HomeQuartersOt3, HomeQuartersOt4, HomeQuartersOt5, AwayCode, AwayName, AwayScore,
         AwayQuartersQ1, AwayQuartersQ2, AwayQuartersQ3, AwayQuartersQ4, AwayQuartersOt1, AwayQuartersOt2, AwayQuartersOt3, AwayQuartersOt4,
         AwayQuartersOt5)

bayern_pan_pbp_list = euroleaguer:: getGamePlayByPlay("E2025", 6)

bayern_pan_pbp = bayern_pan_pbp_list$PlayByPlay

bayern_pan_plays = bayern_pan_pbp |> 
  mutate(Identifier = paste(SeasonCode, GameCode, sep = "_"),
         game_id_play = paste(Identifier, NumberOfPlay, sep = "_")) |> 
  select(Identifier, game_id_play, TeamCode, Player_ID, PlayType, PlayerName, Minute, Markertime, PlayInfo)


bayern_pan_spatial_play = euroleaguer:: getGamePoints("E2025", 6)

bayern_pan_shot_locations = bayern_pan_spatial_play |> 
  mutate(Identifier = paste(SeasonCode, GameCode, sep = "_"),
         game_id_play = paste(Identifier, NumberOfPlay, sep = "_")
         ) |> 
  select(Identifier, game_id_play, Player_ID, Player, Action_ID, Action, Points, CoordX, CoordY, Zone, Fastbreak, SecondChance, PointsOffTurnover,
         Minute, Console, PointsA, PointsB) |> 
  rename("Markertime" = Console,
         "PlayerName" = Player
         )


bayern_pan_spatial_plays = left_join(bayern_pan_plays, bayern_pan_shot_locations, by = c("Identifier", "game_id_play", "Player_ID", "PlayerName", 
                                                                                         "Minute", "Markertime")) |> 
  select(Identifier, game_id_play, TeamCode, Player_ID, PlayType, PlayerName, Minute, Markertime, Action, Points, CoordX, CoordY, Zone, Fastbreak, 
         SecondChance, PointsOffTurnover, PointsA, PointsB) |> 
  rename("pan_pts" = PointsA,
         "mun_pts" = PointsB) |> 
  mutate(
    pan_pts = replace(pan_pts, 1, 0),
    mun_pts = replace(mun_pts, 1, 0)
  ) |> 
  fill(pan_pts, mun_pts, .direction = "down")


redundant_plays = c("TOUT", "TOUT_TV", "OUT", "IN", "CCH")

bayern_pan_spatial_plays_copy = bayern_pan_spatial_plays |> 
  filter(!PlayType %in% redundant_plays)

shooting_zones = bayern_pan_spatial_plays_copy |> 
  group_by(Zone) |> 
  summarise(
       total_shots = n()
  )



bayern_pan_spatial_plays = bayern_pan_spatial_plays_copy |> 
  mutate(
    pan_2_fga = cumsum((PlayType == "2PA" | PlayType == "2PM") & TeamCode == "PAN"),
    pan_2_fgm = cumsum((PlayType == "2PM") & TeamCode == "PAN"),
    mun_2_fga = cumsum((PlayType == "2PA" | PlayType == "2PM") & TeamCode == "MUN"),
    mun_2_fgm = cumsum((PlayType == "2PM") & TeamCode == "MUN"),
    pan_3_fga = cumsum((PlayType == "3PA" | PlayType == "3PM") & TeamCode == "PAN"),
    pan_3_fgm = cumsum((PlayType == "3PM") & TeamCode == "PAN"),
    mun_3_fga = cumsum((PlayType == "3PA" | PlayType == "3PM") & TeamCode == "MUN"),
    mun_3_fgm = cumsum((PlayType == "3PM") & TeamCode == "MUN"),
    pan_fg_attempts = pan_2_fga + pan_3_fga,
    mun_fg_attempts = mun_2_fga + mun_3_fga, 
    pan_fg_made = pan_2_fgm + pan_3_fgm,
    mun_fg_made = mun_2_fgm + mun_3_fgm,
    pan_fg_pct = (pan_fg_made/pan_fg_attempts) * 100,
    mun_fg_pct = (mun_fg_made/mun_fg_attempts) * 100,
    pan_3fg_pct = (pan_3_fgm/pan_3_fga) * 100,
    mun_3fg_pct = (mun_3_fgm/mun_3_fga) * 100,
    pan_fta = cumsum((PlayType == "FTM" | PlayType == "FTA") & TeamCode == "PAN"),
    mun_fta = cumsum((PlayType == "FTM" | PlayType == "FTA") & TeamCode == "MUN"),
    pan_ftm = cumsum((PlayType == "FTM") & TeamCode == "PAN"),
    mun_ftm = cumsum((PlayType == "FTM") & TeamCode == "MUN"),
    pan_ft_pct = (pan_ftm/pan_fta) * 100,
    mun_ft_pct = (mun_ftm/mun_fta) * 100,
    pan_efg_pct = ((pan_fg_made + (0.5 * pan_3_fgm))/pan_fg_attempts) * 100,
    mun_efg_pct = ((mun_fg_made + (0.5 * mun_3_fgm))/mun_fg_attempts) * 100,
    pan_true_shooting_pct = (pan_pts/(2 * pan_fg_attempts + .44 * pan_fta)) * 100,
    mun_true_shooting_pct = (mun_pts/(2 * mun_fg_attempts + .44 * mun_fta)) * 100,
    pan_zone_a_attempts = Zone == "A" & TeamCode == "PAN",
    mun_zone_a_attempts = Zone == "A" & TeamCode == "MUN",
    pan_zone_b_attempts = Zone == "B" & TeamCode == "PAN",
    mun_zone_b_attempts = Zone == "B" & TeamCode == "MUN",
    pan_zone_c_attempts = Zone == "C" & TeamCode == "PAN",
    mun_zone_c_attempts = Zone == "C" & TeamCode == "MUN",
    pan_zone_d_attempts = Zone == "D" & TeamCode == "PAN",
    mun_zone_d_attempts = Zone == "D" & TeamCode == "MUN",
    pan_zone_e_attempts = Zone == "E" & TeamCode == "PAN",
    mun_zone_e_attempts = Zone == "E" & TeamCode == "MUN",
    pan_zone_f_attempts = Zone == "F" & TeamCode == "PAN",
    mun_zone_f_attempts = Zone == "F" & TeamCode == "MUN",
    pan_zone_g_attempts = Zone == "G" & TeamCode == "PAN",
    mun_zone_g_attempts = Zone == "G" & TeamCode == "MUN",
    pan_zone_h_attempts = Zone == "H" & TeamCode == "PAN",
    mun_zone_h_attemtps = Zone == "H" & TeamCode == "MUN",
    pan_zone_i_attempts = Zone == "I" & TeamCode == "PAN",
    mun_zone_i_attempts = Zone == "I" & TeamCode == "MUN"
  )









