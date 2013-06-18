using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using NServiceBus;

namespace NServiceBusExample.PrintLabelService.Contracts.Events
{
    public interface LabelPrinted: IEvent 
    {
        Guid OrderId { get; set; }
    }
}
