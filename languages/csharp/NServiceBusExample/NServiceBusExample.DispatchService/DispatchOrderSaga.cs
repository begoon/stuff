using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using NServiceBus;
using NServiceBus.Saga;
using NServiceBusExample.DispatchService.Contracts;
using NServiceBusExample.PaymentService.Contracts.Events;
using NServiceBusExample.PrintLabelService.Contracts.Events;
using NServiceBusExample.StockManagementService.Contracts.Events;

namespace NServiceBusExample.DispatchService
{
    public class DispatchOrderSaga : Saga<DispatchOrderSagaData>,
        IAmStartedByMessages<StockAllocated>,
        IAmStartedByMessages<PaymentTaken>,
        IHandleMessages<LabelPrinted>
    {
        public IBus Bus { get; set; }

        public override void ConfigureHowToFindSaga()
        {
            ConfigureMapping(saga => saga.OrderId, (StockAllocated message) => message.OrderId);
            ConfigureMapping(saga => saga.OrderId, (PaymentTaken message) => message.OrderId);
            ConfigureMapping(saga => saga.OrderId, (LabelPrinted message) => message.OrderId);
        }

        public void Handle(StockAllocated message)
        {
            Data.OrderId = message.OrderId;
            Data.StockAllocated = true;

            PrintPostageLabel();
        }

        public void Handle(PaymentTaken message)
        {
            Data.OrderId = message.OrderId;
            Data.PaymentTaken = true;

            PrintPostageLabel();
        }

        public void Handle(LabelPrinted message)
        {
            Console.WriteLine("Label printed, dispatch complete.");

            MarkAsComplete();
        }

        private void PrintPostageLabel()
        {
            if (Data.PaymentTaken && Data.StockAllocated)
            {
                Bus.Publish<OrderInStockPaymentTaken>( e => e.OrderId = Data.OrderId );
            }
        }
    }
}
