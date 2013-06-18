using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using NServiceBus;

namespace NServiceBusExample.OrderService.Contracts.Commands
{
    public class PlaceOrder : ICommand
    {
        public List<int> ProductCodes { get; set; }
    }
}
