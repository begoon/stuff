using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using NServiceBus;
using NServiceBusExample.DispatchService.Contracts;
using NServiceBusExample.PrintLabelService.Contracts.Events;

namespace NServiceBusExample.PrintLabelService
{
    public class PrintLabelReadyToPrintHandler : IHandleMessages<OrderInStockPaymentTaken>
    {
        public IBus Bus { get; set; }
        public void Handle(OrderInStockPaymentTaken message)
        {
            Console.WriteLine("Print label for order: " + message.OrderId);

            Bus.Publish<LabelPrinted>(e => { e.OrderId = message.OrderId; });
        }
    }
}
