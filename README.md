# IPL_Analysis_automation

![image](https://github.com/chandranshuanalyst/IPL_Analysis_automation/assets/91171166/9124e868-f8fb-4dfa-84ba-4a1594744775)

## Welcome to my IPL analysis project!!!

The aim of this project is to automate the team and individual players records in IPL (2008-2020),
so that we can retrive their stats by just calling the required functions.

#### The Key functions are :
1. team : It is a function which returns all statistics about a team including :
   * top run scorers
   * most destructive batsman
   * top 4 hitters
   * top 6 hitters
   * top wicket takers
   * top strike bowlers
   * most economical bowlers
   * the poor bowlers
   It also provides information about:
   * Top run scorers by season
   * Top wicket takers by season
   We also get to know about the impact players for each team.
2. over_analysis : It returns the top run scorers for each over taking team name as parameter .
3. position_runs : It returns the top run scorers for each position taking team name as parameter .
4. tvt : It returns the analysis of one team against other,
   It involves two functions:
   *batting_vsteam : It returns the batting records versus a bowling team .
   *bowling_vsteam : It returns the bowling records versus a batting team .
5. batter_perf : It takes the name of batter as input and returns all statistics about a batter.
It includes functions :
    * batsman_performance : It returns overall performance of batsman
    * position_wise       : It returns position wise analysis of batsman
    * inning_wise         : It returns innings wise analysis of batsman
    * season_wise         : It returns season wise analysis of batsman
    * high_risk           : It returns bowlers that have upper hand on batsman
    * low_risk            : It returns bowlers that have lower hand on batsman
    * venue_wise          : It returns the best batting performances by venue
6. bowler_perf : It takes the name of bowler as input and returns all statistics about a bowler.
It includes functions :
    * bowling_perf        : It returns overall performance of bowler
    * bowler_inning       : It returns innings wise analysis of bowler
    * bowler_season       : It returns season wise analysis of bowler
    * danger_batter       : It returns batters that have upper hand on bowler
    * easy_batter         : It returns batters that have lower hand on bowler
    * best_venue          : It returns the best bowling performances by venue
