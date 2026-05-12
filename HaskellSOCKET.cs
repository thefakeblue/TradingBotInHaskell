// ===================== NINJATRADER STRATEGY (C#) =====================

using System;
using System.Net.Sockets;
using System.Text;
using NinjaTrader.Cbi;
using NinjaTrader.NinjaScript;
using NinjaTrader.NinjaScript.Strategies;
using NinjaTrader.NinjaScript.Indicators;

namespace NinjaTrader.NinjaScript.Strategies
{
    public class HaskellSocketEMA : Strategy
    {
        private TcpClient client;
        private NetworkStream stream;

        private EMA emaFast;
        private EMA emaSlow;

        protected override void OnStateChange()
        {
            if (State == State.SetDefaults)
            {
                Name = "HaskellSocketEMA";
                Calculate = Calculate.OnBarClose;
            }
            else if (State == State.DataLoaded)
            {
                emaFast = EMA(10); // chart indicators only, not sent to Haskell
                emaSlow = EMA(25);

                AddChartIndicator(emaFast);
                AddChartIndicator(emaSlow);

                try
                {
                    client = new TcpClient("127.0.0.1", 5001);
                    stream = client.GetStream();
                    Print("Connected to Haskell server");
                }
                catch (Exception e)
                {
                    Print("Connection failed: " + e.Message);
                }
            }
            else if (State == State.Terminated)
            {
                stream?.Close();
                client?.Close();
            }
        }

        protected override void OnBarUpdate()
        {
            if (CurrentBar < 10) return;
            if (stream == null) return;

            double open  = Open[0];
            double high  = High[0];
            double low   = Low[0];
            double close = Close[0];

            string timestamp = Time[0].ToString("o"); // ISO 8601

            string message =
                timestamp + "," +
                open + "," +
                high + "," +
                low + "," +
                close + "\n";

            byte[] data = Encoding.UTF8.GetBytes(message);

            try
            {
                stream.Write(data, 0, data.Length);

                byte[] buffer = new byte[256];
                int bytes = stream.Read(buffer, 0, buffer.Length);

                string response =
                    Encoding.UTF8.GetString(buffer, 0, bytes).Trim();

                Print("Haskell says: " + response);

                if (response.Contains("BUY") &&
                    Position.MarketPosition != MarketPosition.Long)
                {
                    EnterLong();
                }
                else if (response.Contains("SELL") &&
                         Position.MarketPosition != MarketPosition.Short)
                {
                    EnterShort();
                }
                else if (response.Contains("CLOSE"))
                {
                    // Exit the current position and go flat — no new entry.
                    // Haskell will send BUY or SELL on the next candle if a
                    // re-entry is scheduled (same-direction "repo" logic).
                    if (Position.MarketPosition == MarketPosition.Long)
                        ExitLong();
                    else if (Position.MarketPosition == MarketPosition.Short)
                        ExitShort();
                }
            }
            catch (Exception e)
            {
                Print("Socket error: " + e.Message);
            }
        }
    }
}
