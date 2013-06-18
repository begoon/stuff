using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using NServiceBus;

namespace NServiceBusExample.PaymentService.Contracts.Events
{
    public interface PaymentTaken : IEvent
    {
        Guid OrderId { get; set; }
    }
}
