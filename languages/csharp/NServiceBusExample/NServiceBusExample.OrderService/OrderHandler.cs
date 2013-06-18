using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;
using NServiceBus;
using NServiceBusExample.OrderService.Contracts.Commands;
using NServiceBusExample.OrderService.Contracts.Events;

namespace NServiceBusExample.OrderService
{
    public class OrderHandler : IHandleMessages<PlaceOrder>
    {
        public IBus Bus { get; set; }

        public void Handle(PlaceOrder message)
        {
            Console.WriteLine("Order received, number of items in order: " + message.ProductCodes.Count);

            Bus.Publish<OrderPlaced>(e=>
            {
                e.OrderId = Guid.NewGuid();
                e.ProductCodes = message.ProductCodes;
            });
        }
    }
}
