using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using NServiceBus;

namespace NServiceBusExample.DispatchService.Contracts
{
    public interface OrderInStockPaymentTaken: IEvent 
    {
        Guid OrderId { get; set; }
    }
}
