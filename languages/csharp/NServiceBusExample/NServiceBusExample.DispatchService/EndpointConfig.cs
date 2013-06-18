namespace NServiceBusExample.DispatchService 
{
    using NServiceBus;

	/*
		This class configures this endpoint as a Server. More information about how to configure the NServiceBus host
		can be found here: http://particular.net/articles/profiles-for-nservicebus-host
	*/
	public class EndpointConfig : IWantCustomInitialization,IConfigureThisEndpoint, AsA_Publisher
    {
	    public void Init()
	    {
	        Configure.With().DefaultBuilder().RavenPersistence().RavenSagaPersister().RavenSubscriptionStorage();
	    }
    }
}