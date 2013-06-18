using System;
using System.Collections.Generic;
using System.Linq;
using System.Messaging;
using System.Text;
using System.Threading;
using NServiceBus;
using NServiceBusExample.OrderService.Contracts.Commands;

namespace NServiceBusExample.Client
{
    class Program
    {
        private static readonly List<String> RequiredQueues = new List<string>()
            {
                "private$\\nservicebusexample.orderservice",
                "private$\\nservicebusexample.correspondanceservice",
                "private$\\nservicebusexample.stockmanagementservice",
                "private$\\nservicebusexample.paymentservice",
                "private$\\nservicebusexample.dispatchservice",
                "private$\\nservicebusexample.printlabelservice",
            };

        static void Main(string[] args)
        {
            NServiceBus.SetLoggingLibrary.Log4Net();

            IBus bus = Configure.With()
                .DefaultBuilder()
                .Log4Net()
                .MsmqTransport()
                .IsTransactional(true)
                .PurgeOnStartup(true)
                .UnicastBus()
                .ImpersonateSender(false)
                .LoadMessageHandlers()
                .SendOnly();

            WaitForQueues();

            Console.WriteLine("Placing order...");

            bus.Send<PlaceOrder>(c => { c.ProductCodes = new List<int> {1, 2, 3}; });

            Console.WriteLine("Order placed");
            Console.Read();
        }

        private static void WaitForQueues()
        {
            //This method is a bit of a hack.  The first time you run this solution the queues won't exist.  The NServiceBus.Host.exe will create the queues
            //as the processes are started up.  The queues will then be present until manually removed.  We just need to wait (the first time) to give 
            //NServiceBus.Host a chance to create the queues before trying to send messages.

            bool queuesExist = false;
            while(!queuesExist)
            {
                queuesExist = RequiredQueues.All(rq=> MessageQueue.GetPrivateQueuesByMachine("localhost").Select(m=>m.QueueName).Any(mq=>string.Equals(mq, rq, StringComparison.OrdinalIgnoreCase)));
                
                if (!queuesExist)
                {
                    Thread.Sleep(5000);
                }
            }
        }
    }
}
