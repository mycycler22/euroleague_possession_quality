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


bayern_pan_spatial_plays_copy = bayern_pan_spatial_plays

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
    pan_fg_attempts = pan_2_fga + pan_3_fga
  )









