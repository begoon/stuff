using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using NServiceBus;
using NServiceBusExample.OrderService.Contracts.Events;
using NServiceBusExample.PaymentService.Contracts.Events;

namespace NServiceBusExample.PaymentService
{
    public class OrderPlacedHandler : IHandleMessages<OrderPlaced>
    {
        public IBus Bus { get; set; }
        
        public void Handle(OrderPlaced message)
        {
            Console.WriteLine("Taking payment for order: " + message.OrderId);

            Bus.Publish<PaymentTaken>(e => { e.OrderId = message.OrderId; });
        }
    }
}
