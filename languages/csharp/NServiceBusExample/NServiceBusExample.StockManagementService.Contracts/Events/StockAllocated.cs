using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using NServiceBus;

namespace NServiceBusExample.StockManagementService.Contracts.Events
{
    public interface StockAllocated : IEvent
    {
        Guid OrderId { get; set; }
    }
}
