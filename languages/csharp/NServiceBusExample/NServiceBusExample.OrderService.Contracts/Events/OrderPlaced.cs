using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using NServiceBus;

namespace NServiceBusExample.OrderService.Contracts.Events
{
    public interface OrderPlaced : IEvent
    {
        Guid OrderId { get; set; }
        List<int> ProductCodes { get; set; }
    }
}
