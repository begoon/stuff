using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using NServiceBus;
using NServiceBusExample.OrderService.Contracts.Events;
using NServiceBusExample.StockManagementService.Contracts.Events;

namespace NServiceBusExample.StockManagementService
{
    public class OrderPlacedHandler : IHandleMessages<OrderPlaced>
    {
        public IBus Bus { get; set; }

        public void Handle(OrderPlaced message)
        {
            Console.WriteLine("Allocating stock for order:" + message.OrderId);

            Bus.Publish<StockAllocated>(e => { e.OrderId = message.OrderId; });
        }
    }
}
