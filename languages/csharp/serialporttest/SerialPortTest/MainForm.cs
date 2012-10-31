using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;

namespace SerialPortTest
{
	public partial class MainForm : Form
	{
		public MainForm()
		{
			InitializeComponent();

			CheckForIllegalCrossThreadCalls = false;

			/// Open port
			serialPort.Open();

			/// Add event handler for incoming data
			serialPort.DataReceived += new System.IO.Ports.SerialDataReceivedEventHandler(this.onSerialPortReceived);
		}

		private void onSerialPortReceived(object sender, EventArgs e)
		{
			/// Reading everying from port buffer
			string data = serialPort.ReadExisting();
			/// Add string to the Text box
			logTextBox.Text += data;
		}

		private void goButton_Click(object sender, EventArgs e)
		{
			// Send command to the modem
			serialPort.Write("ATI4\r");
		}
	}
}