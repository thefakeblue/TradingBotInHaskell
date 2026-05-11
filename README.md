# TradingBotInHaskell
A Haskell trading bot for strategy backtesting and live replay.

## Replay harness (`app/CsvReplay.hs`)
Use `app/CsvReplay.hs` to test strategies against historical CSV data.

### How to run
1. Open `app/CsvReplay.hs`.
2. Uncomment the data file you want to test:
   - `1YearHistoricalData1Min.csv`
   - `1YearHistoricalData3Min.csv`
   - `1YearHistoricalData5Min.csv`
   - `1YearHistoricalData10Min.csv`
3. Uncomment a strategy and comment out the others.
4. Run the replay with:
   ```bash
   cabal run csv-replay
   ```

### Available strategies
- `simpleStrategy`
- `momentumStrategy 0.002`
- `meanReversionStrategy 0.002`
- `rangeBreakoutStrategy 0.25`
- `customTrendBreakoutStrategy 0.25 0.002`
- `customRangeReversionStrategy 0.45 0.25`
- `customRangeReversionConservative 0.45 0.25`
- `stepRSIStrategy 30 70`
- `stepMAStrategy 9 21`
- `stepEMABreakoutStrategy 12`

### What it prints
The replay outputs:
- data file
- strategy name
- net profit
- gross profit / loss
- profit factor
- win rate
- win/loss counts
- total trades
- final cash

### Notes
- The CSV parser ignores the original action column from the historical file and recomputes decisions for the selected strategy.
- Only one data file and one strategy should be active at a time.
- To compare multiple options, uncomment one configuration, run the replay, then comment it back and choose the next.

