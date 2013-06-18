using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using NServiceBus.Saga;

namespace NServiceBusExample.DispatchService
{
    public class DispatchOrderSagaData : ISagaEntity
    {

        public Guid OrderId { get; set; }
        public bool StockAllocated { get; set; }
        public bool PaymentTaken { get; set; }
        public bool LabelPrinted { get; set; }

        public bool CanBeDispatched { get { return StockAllocated && PaymentTaken && LabelPrinted; } }

        public Guid Id { get; set; }

        public string Originator { get; set; }

        public string OriginalMessageId { get; set; }
    }
}
