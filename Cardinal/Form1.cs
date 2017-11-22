using System;
using System.Windows.Forms;
using System.Data.SqlClient;
using System.Windows.Threading;

namespace Cardinal
{
    public partial class Form1 : Form
    {
        SqlDependency sqlDependencyStalker;
        SqlConnection conn;
        SqlCommand SC;
        SqlDataReader SDR;
        Dispatcher Dis = Dispatcher.CurrentDispatcher;
        string log_caption, group, str_connect = "user id=ldo;password=IfLyyz4sCJ;server=nitel-hp;database=uit;MultipleActiveResultSets=True";
        bool is_visual = false;
        string[] groups_and_users;

        public Form1()
        {
            InitializeComponent();
            conn = new SqlConnection(str_connect);
            conn.Open();
            SqlDependency.Start(str_connect);
            Stalker();
            NI.Icon = Cardinal.Properties.Resources.eye_alien;
        }

        void Stalker()
        {
            if (sqlDependencyStalker != null)
            {
                sqlDependencyStalker.OnChange -= OnDatabaseChange_Id_and_nom_ceh;
                sqlDependencyStalker = null;
            }
            using (var command = new SqlCommand("select naim from dbo.asup_log", conn))
            {
                sqlDependencyStalker = new SqlDependency(command);
                sqlDependencyStalker.OnChange += new OnChangeEventHandler(OnDatabaseChange_Id_and_nom_ceh);
                command.ExecuteReader();
            }
        }

        private void NI_MouseDoubleClick(object sender, MouseEventArgs e)
        {
            Dis.Invoke(new Action(FormVisible));
        }

        protected override void OnShown(System.EventArgs e)
        {
            base.OnShown(e);
            Hide();
        }

        void OnDatabaseChange_Id_and_nom_ceh(object sender, SqlNotificationEventArgs e)
        {
            SC = new SqlCommand("select top 1 * from dbo.asup_log order by dt desc", conn);
            SDR = SC.ExecuteReader();
            while (SDR.Read())
            {
                log_caption = SDR.GetDateTime(0).ToShortTimeString() + "  " + SDR.GetString(1);
                if (!SDR.IsDBNull(2)) group = SDR.GetString(2);
                else group = "";
            }
            if (group.Trim().Length != 0)
            {
                groups_and_users = group.Split(',');
                for (int i = 0; i < groups_and_users.Length; i++)
                {
                    SC = new SqlCommand("select * from cvodka.dbo.log_asup_ where vxod is null and loginn = '" + groups_and_users[i] + "'", conn);
                    SDR = SC.ExecuteReader();
                    if (SDR.HasRows)
                    {
                        SC = new SqlCommand("select * from (select l2.loginn as groupn,l.loginn as polz from [cvodka].[dbo].[group_users] gu inner join "
                                            + "cvodka.dbo.log_asup_ l on l.id=gu.id_u inner join cvodka.dbo.log_asup_ l2 on l2.id=gu.id_g) as tmp where"
                                            + " groupn = '" + groups_and_users[i] + "' and polz = '" + Environment.UserName + "'", conn);
                        SDR = SC.ExecuteReader();
                        if (SDR.HasRows) is_visual = true;
                    }
                    else
                    if (Environment.UserName == groups_and_users[i]) is_visual = true;
                }
            }
            else is_visual = true;
            if (is_visual)
            {
                Dis.Invoke(new Action(FormVisible));
                Dis.Invoke(new Action(AddInList));
                Stalker();
            }
            is_visual = false;
        }

        private void Form1_FormClosing(object sender, FormClosingEventArgs e)
        {
            if (e.CloseReason != CloseReason.WindowsShutDown)
            {
                e.Cancel = true;
                Dis.Invoke(new Action(FormInvisible));
            }
        }

        void FormVisible()
        {
            Visible = true;
            NI.Visible = false;
        }

        void FormInvisible()
        {
            Visible = false;
            NI.Visible = true;
        }
       
        void AddInList()
        {
            listBox1.Items.Add(log_caption);
        }

    }
}
