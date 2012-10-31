namespace SerialPortTest
{
	partial class MainForm
	{
		/// <summary>
		/// Required designer variable.
		/// </summary>
		private System.ComponentModel.IContainer components = null;

		/// <summary>
		/// Clean up any resources being used.
		/// </summary>
		/// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
		protected override void Dispose(bool disposing)
		{
			if (disposing && (components != null))
			{
				components.Dispose();
			}
			base.Dispose(disposing);
		}

		#region Windows Form Designer generated code

		/// <summary>
		/// Required method for Designer support - do not modify
		/// the contents of this method with the code editor.
		/// </summary>
		private void InitializeComponent()
		{
			this.components = new System.ComponentModel.Container();
			this.goButton = new System.Windows.Forms.Button();
			this.logTextBox = new System.Windows.Forms.TextBox();
			this.portTextBox = new System.Windows.Forms.TextBox();
			this.serialPort = new System.IO.Ports.SerialPort(this.components);
			this.SuspendLayout();
			// 
			// goButton
			// 
			this.goButton.Location = new System.Drawing.Point(12, 221);
			this.goButton.Name = "goButton";
			this.goButton.Size = new System.Drawing.Size(75, 23);
			this.goButton.TabIndex = 0;
			this.goButton.Text = "Go";
			this.goButton.UseVisualStyleBackColor = true;
			this.goButton.Click += new System.EventHandler(this.goButton_Click);
			// 
			// logTextBox
			// 
			this.logTextBox.Location = new System.Drawing.Point(12, 12);
			this.logTextBox.Multiline = true;
			this.logTextBox.Name = "logTextBox";
			this.logTextBox.Size = new System.Drawing.Size(255, 155);
			this.logTextBox.TabIndex = 1;
			// 
			// portTextBox
			// 
			this.portTextBox.Location = new System.Drawing.Point(12, 184);
			this.portTextBox.Name = "portTextBox";
			this.portTextBox.Size = new System.Drawing.Size(255, 20);
			this.portTextBox.TabIndex = 2;
			this.portTextBox.Text = "COM4";
			// 
			// serialPort
			// 
			this.serialPort.DtrEnable = true;
			this.serialPort.PortName = "COM4";
			this.serialPort.RtsEnable = true;
			// 
			// MainForm
			// 
			this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
			this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
			this.ClientSize = new System.Drawing.Size(292, 266);
			this.Controls.Add(this.portTextBox);
			this.Controls.Add(this.logTextBox);
			this.Controls.Add(this.goButton);
			this.Name = "MainForm";
			this.Text = "Form1";
			this.ResumeLayout(false);
			this.PerformLayout();

		}

		#endregion

		private System.Windows.Forms.Button goButton;
		private System.Windows.Forms.TextBox logTextBox;
		private System.Windows.Forms.TextBox portTextBox;
		private System.IO.Ports.SerialPort serialPort;
	}
}

