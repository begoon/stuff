using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using NServiceBus;
using NServiceBusExample.OrderService.Contracts.Events;

namespace NServiceBusExample.CorrespondanceService
{
    public class OrderPlacedHandler : IHandleMessages<OrderPlaced>
    {
        public void Handle(OrderPlaced message)
        {
            Console.WriteLine("Sending email for order Id: " + message.OrderId);
        }
    }
}
