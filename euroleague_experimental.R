
library(euroleaguer)
library(tidyverse)
library(dplyr)


comp_history = euroleaguer::getCompetitionHistory("E")

head(comp_history)

euroleague_25_teams = euroleaguer::getCompetitionTeams("E2025")
euroleague_25_phases = euroleaguer::getCompetitionPhases("E2025")
euroleague_25_rounds = euroleaguer::getCompetitionRounds("E2025")

euroleague_25_regular_season_rounds = euroleague_25_rounds |> 
  filter(PhaseTypeCode == "RS") |> 
  select(SeasonCode, PhaseTypeCode, Round, Index, Name)

euroleague_25_rounds_vector = euroleague_25_regular_season_rounds$Round

euroleague_25_rs_games = euroleaguer::getCompetitionGames("E2025", euroleague_25_rounds_vector, phase_type = "RS") |> 
  select(SeasonCode, Round, PhaseType, Identifier, GameCode, HomeCode, HomeName, HomeScore, HomeQuartersQ1, HomeQuartersQ2, HomeQuartersQ3, 
         HomeQuartersQ4, HomeQuartersOt1, HomeQuartersOt2, HomeQuartersOt3, HomeQuartersOt4, HomeQuartersOt5, AwayCode, AwayName, AwayScore,
         AwayQuartersQ1, AwayQuartersQ2, AwayQuartersQ3, AwayQuartersQ4, AwayQuartersOt1, AwayQuartersOt2, AwayQuartersOt3, AwayQuartersOt4,
         AwayQuartersOt5)

euroleague_25_rs_game_codes = euroleague_25_rs_games$GameCode

euroleague_25_rs_play_by_play = euroleaguer::getGamePlayByPlay("E2025", euroleague_25_rs_game_codes)

euroleague_25_rs_plays = euroleague_25_rs_play_by_play$PlayByPlay

euroleague_25_rs_plays = euroleague_25_rs_plays |> 
  group_by(SeasonCode, GameCode) |> 
  mutate(Identifier = paste(SeasonCode, GameCode, sep = "_"),
         game_id_play = paste(Identifier, NumberOfPlay, sep = "_")
         ) |> 
  select(SeasonCode, GameCode, Identifier, game_id_play, Quarter, TeamCode, Player_ID, PlayType, PlayerName, Minute, Markertime, 
         PointsA, PointsB, PlayInfo)

euroleague_25_rs_plays$Type = NULL


euroleague_25_player_advanced = euroleaguer:: getPlayerAdvanced("E2025")

euroleague_25_player_stats = euroleaguer:: getPlayerStats("E2025")

euroleague_25_player_misc = euroleaguer:: getPlayerMisc("E2025")





