"""
Developed by Azhagan avr (aavr@kittelson.com)
"""

# Import Libraries
import numpy as np
import pandas as pd
pd.plotting.deregister_matplotlib_converters()
from matplotlib.cm import ScalarMappable
import matplotlib.colors as mc  # For the legend
import matplotlib.pyplot as plt
import matplotlib
matplotlib.use('Agg')


def input_file_path():
    
    data_file = r"C:\Users\aavr\Python Projects\Speed Heatmaps\Data\InrixXD_TN_SR_109_Apr2021_Mar2022_SB\InrixXD_TN_SR_109_Apr2021_Mar2022_SB.csv"
    id_file = r"C:\Users\aavr\Python Projects\Speed Heatmaps\Data\InrixXD_TN_SR_109_Apr2021_Mar2022_SB\XD_Identification.csv"
    output = r"C:\Users\aavr\Python Projects\Speed Heatmaps\Results\TN\SB All\\"
    # Discontinued
    id_xd = [449621630,1524370422,1524496178,1524543702,1524351079,1524337040,449617972,1524336919,1524336942] # TN SB
    segments_selected = 9

    return data_file, id_file, id_xd, output, segments_selected


def read_input_file(id_file, data_file, id_list):
    df = pd.read_csv(data_file, low_memory=False)
    # print(df["xd_id"].unique().tolist())
    id_df = pd.read_csv(id_file, low_memory=False)
    id_df.sort_values(by="road_order", ascending=True, inplace=True)
    id_list = id_df["xd"].tolist()

    df = df[df["xd_id"].isin(id_list)]
    df["measurement_tstamp"] = pd.to_datetime(df["measurement_tstamp"])
    df["day"] = df["measurement_tstamp"].dt.day
    df["hour"] = df["measurement_tstamp"].dt.hour
    df["date"] = df["measurement_tstamp"].dt.date
    df["time"] = df["measurement_tstamp"].dt.time
    df["month"] = pd.DatetimeIndex(df["measurement_tstamp"]).month
    df["dayofweek"] = df["measurement_tstamp"].dt.dayofweek + 1
    df["week"] = df["measurement_tstamp"].dt.week + 1
    df["start_time"] = df['measurement_tstamp'].dt.to_period('W').dt.start_time
    df["start_time"] = pd.to_datetime(df["start_time"])
    df["start_date"] = df['start_time'].dt.date

    return id_df, df


def percentile(n):
    def percentile_(x):
        return np.percentile(x, n)
    percentile_.__name__ = 'percentile_%s' % n
    return percentile_


def data_agg(id_df, df, idx, col, pivot):
    df_perc = df.groupby(["xd_id"]).agg({"speed": [percentile(5), percentile(15), "median", percentile(85), percentile(95)],
                                         "travel_time_seconds": [percentile(15), percentile(5)]})
    df_perc.columns = df_perc.columns.map('|'.join).str.strip('|')
    df_perc.reset_index(inplace=True)
    df_perc = df_perc.rename(columns={'speed|percentile_5': 'speed_5th', 'speed|percentile_15': 'speed_15th',
                                      'speed|median': 'speed_median', 'speed|percentile_85': 'speed_85th',
                                      'speed|percentile_95': 'speed_95th',
                                      'travel_time_seconds|percentile_5': 'travel_time_seconds_5th',
                                      'travel_time_seconds|percentile_15': 'travel_time_seconds_15th'})

    df_grouped = df.merge(df_perc, how="left", on="xd_id")
    df_grouped["speed_diff_median"] = df_grouped['speed_median'] - df_grouped["speed"]
    df_grouped["speed_diff_85"] = df_grouped['speed_85th'] - df_grouped["speed"]
    df_grouped["speed_diff_95"] = df_grouped['speed_95th'] - df_grouped["speed"]
    df_grouped["tti_15"] = df_grouped["travel_time_seconds"] / df_grouped["travel_time_seconds_15th"]
    df_grouped["tti_5"] = df_grouped["travel_time_seconds"] / df_grouped["travel_time_seconds_5th"]
    df_grouped["speedi_85"] = df_grouped['speed'] / df_grouped["speed_85th"]
    df_grouped["speedi_95"] = df_grouped['speed'] / df_grouped["speed_95th"]
    df_grouped_2 = df_grouped.groupby(["xd_id", idx, col]).agg({"speed": ["mean"], "speed_diff_median": ["mean"],
                                                              "speed_diff_85": ["mean"], "speed_diff_95": ["mean"],
                                                              "tti_5": ["mean"], "tti_15": ["mean"],
                                                              "speedi_85": ["mean"], "speedi_95": ["mean"]})
    df_grouped_2.columns = df_grouped_2.columns.map('|'.join).str.strip('|')
    df_grouped_2.reset_index(inplace=True)
    df_grouped_2 = df_grouped_2.rename(columns={"xd_id": "xd", 'speed|mean': 'speed',
                                            'speed_diff_median|mean': 'speed_diff_median',
                                            'speed_diff_85|mean': 'speed_diff_85',
                                            'speed_diff_95|mean': 'speed_diff_95', 'tti_5|mean': 'tti_5',
                                            'tti_15|mean': 'tti_15', "speedi_85|mean": "speedi_85",
                                            "speedi_95|mean": "speedi_95"})

    df_grouped_2 = df_grouped_2.merge(id_df, how="left", on="xd")
    # df_grouped_2["delay_spm_85"] = 3600 * ((df_grouped_2["miles"] / df_grouped_2["speed"]) -
    #                                   (df_grouped_2["miles"] / df_grouped_2['speed_diff_85']))
    # print(df_grouped_2[["speed", "speed_85th", "delay_spm"]].describe())

    if pivot:
        print(pd.pivot_table(df_grouped_2, index=[idx], columns=[col], values='speed').reset_index())
        return df_grouped, df_grouped_2
    else:
        return df_grouped, df_grouped_2


def create_heatmap(df_grouped, xd_id_list, output,  min_lim, max_lim, cmap, value, xticks, cmap_label):
    print("Creating Heatmaps ...")
    for i in range(0, segments):

        ip_df_plot = df_grouped[df_grouped["xd"] == xd_id_list[i]].reset_index()
        print("XD ID:", xd_id_list[i])

        # date = len(ip_df_plot["date"].unique())
        # hour = len(ip_df_plot["hour"].unique())
        # time = ip_df_plot["speed"].values.reshape(19,260, order="F")

        table = pd.pivot_table(ip_df_plot, index=['hour'], columns=['date'], values=value)
        time = table.to_numpy()
        xgrid = np.arange(len(ip_df_plot["date"].unique().tolist()) + 1)
        ygrid = np.arange(len(ip_df_plot["hour"].unique().tolist()) + 1)

        c_map = plt.cm.get_cmap(cmap)
        fig, ax = plt.subplots(figsize=(15, 7.5))
        ax.pcolormesh(xgrid, ygrid, time, cmap=c_map, vmin=min_lim, vmax=max_lim)

        ax.yaxis.set_ticks([i for i in range(len(ip_df_plot["hour"].unique().tolist()))])
        ax.set_yticklabels(ip_df_plot["hour"].unique().tolist())
        ax.set_ylabel("Time of Day")

        ax.xaxis.set_ticks([i for i in range(len(ip_df_plot["date"].unique().tolist()))][::xticks])
        xlabels = ip_df_plot["date"].unique().tolist()
        ax.set_xticklabels(xlabels[::xticks], rotation=90)
        ax.set_xlabel("Date")

        # ax.yaxis.set_tick_params(length=0)
        # ax.xaxis.set_tick_params(length=0)
        # ax.set_frame_on(False) # remove all spines

        fig.subplots_adjust(bottom=0.15)
        norm = mc.Normalize(min_lim, max_lim)
        cb = fig.colorbar(
            ScalarMappable(norm=norm, cmap=c_map),
            # cax=cbar_ax, # Pass the new axis
            # orientation = "horizontal"
        )
        cb.set_label(cmap_label, size=12)

        try:
            title = "Seg: " + str(i+1) + ", " + str(ip_df_plot["road-name"][0]) + " ," + str(ip_df_plot["county"][0]) + \
                    " COUNTY, " + str(ip_df_plot["state"][0]) + ", " + str(ip_df_plot["zip"][0]) + " - XD Id: " + \
                    str(ip_df_plot["xd"][0]) + ", LEN: " + str(round(ip_df_plot["miles"][0], 2))+" miles"

            print(title)
            plt.title(title)
        except Exception as err:
            pass

        plt.savefig(output + value + "\\" + value + "_" + str(i+1)+".png", bbox_inches='tight', pad_inches=0.25)
        # plt.show()
        plt.close()


def create_tti(df, xd_id_list, output, tt_idx, name):
    print("Creating " + name + "Index Reliability Plot ...")
    df = df.groupby(["xd", "hour"]).agg({tt_idx: ["mean"]})
    df.columns = df.columns.map('|'.join).str.strip('|')
    df.reset_index(inplace=True)
    df = df.rename(columns={tt_idx + '|mean': tt_idx})
    df[tt_idx] = df[tt_idx].apply(lambda x: round(x, 2))

    df = df.groupby(["xd", tt_idx]). agg({tt_idx: ["count"]})
    df.columns = df.columns.map('|'.join).str.strip('|')
    df.reset_index(inplace=True)
    df = df.rename(columns={tt_idx + '|count': 'count'})
    bins = np.linspace(0, 2, num=21)
    for i in range(0, segments):
        df_print = df[df["xd"] == xd_id_list[i]]
        df_print.reset_index(inplace=True)
        count, bins_count = np.histogram(df_print[tt_idx], bins=bins)
        # print(count, bins_count)
        pdf = count / sum(count) * 100
        cdf = np.cumsum(pdf)
        # print(pdf)
        # print(cdf)

        plt.figure(figsize=(8, 6))
        plt.plot(bins_count[1:], cdf, label="CDF", marker="o")
        plt.scatter(bins_count[1:], cdf, marker="o")
        plt.legend(loc="best", fontsize=8)
        plt.xlabel(name + " Index", fontsize=12)
        plt.ylabel("Percentage %", fontsize=12)
        plt.title(name + " Index Reliability Plot Seg: " + str(i+1) + " XD: " + str(xd_id_list[i]))
        plt.grid()
        plt.ylim(0, 100)
        plt.xticks(bins_count[1:].tolist(), [round(num, 1) for num in bins[1:]])
        plt.savefig(output + tt_idx + "\\" + tt_idx + "_CDF_" + str(i + 1) + ".png", bbox_inches='tight', pad_inches=0.25)
        # plt.close()
    return df


def create_tti_non_agg(df, xd_id_list, output, tt_idx, name):
    print("Creating " + name + "Index Reliability Plot ...")
    bins = np.linspace(0, 2, num=210000)
    for i in range(0, segments):
        df_print = df[df["xd_id"] == xd_id_list[i]]
        df_print.reset_index(inplace=True)
        count, bins_count = np.histogram(df_print[tt_idx], bins=bins)
        # print(count, bins_count)
        pdf = count / sum(count) * 100
        cdf = np.cumsum(pdf)
        # print(pdf)
        # print(cdf)

        plt.figure(figsize=(8, 6))
        plt.plot(bins_count[1:], cdf, label="CDF")
        # plt.scatter(bins_count[1:], cdf, marker="o")
        plt.legend(loc="best", fontsize=8)
        plt.xlabel(name + " Index", fontsize=12)
        plt.ylabel("Percentage %", fontsize=12)
        plt.title(name + " Index Reliability Plot Seg: " + str(i+1) + " XD: " + str(xd_id_list[i]))
        plt.grid()
        # plt.xticks(bins_count[1:].tolist(), [round(num, 1) for num in bins[1:]])
        plt.xticks(np.arange(0, 2.1, 0.1))
        plt.yticks(np.arange(0, 110, 10))
        plt.ylim(0, 100)
        # plt.xlim(left=0)
        plt.xlim(1, 2)
        plt.savefig(output + tt_idx + "_CDF_" + str(i + 1) + ".png", bbox_inches='tight', pad_inches=0.25)
        # plt.close()
    return df


def create_confidence_band_hourly(df, xd_id_list, output, lower_df, upper_df, name):
    xaxis = "hour"
    df_perc = df.groupby(["xd_id", xaxis]).agg({"speed": [percentile(lower_df), percentile(upper_df), "median"]})
    df_perc.columns = df_perc.columns.map('|'.join).str.strip('|')
    df_perc.reset_index(inplace=True)
    df_perc = df_perc.rename(columns={'speed|percentile_' + str(lower_df): 'speed_' + str(lower_df) + 'th',
                                      'speed|percentile_' + str(upper_df): 'speed_' + str(upper_df) + 'th',
                                      'speed|median': 'speed_median'})

    print("Creating " + name + " Confidence Band Plot ...")
    bins = np.linspace(0, 23, num=24)
    for i in range(0, segments):
        df_print = df_perc[df_perc["xd_id"] == xd_id_list[i]]
        df_print.reset_index(inplace=True)
        plt.figure()
        plt.plot(df_print[xaxis], df_print['speed_' + str(lower_df) + 'th'], label=str(lower_df) + "th %ile", marker="o")
        plt.plot(df_print[xaxis], df_print["speed_median"], label="median", marker="*")
        plt.plot(df_print[xaxis], df_print['speed_' + str(upper_df) + 'th'], label=str(upper_df) + "th %ile", marker="o")
        plt.legend(loc="best", fontsize=8)
        plt.xlabel("Time of Day", fontsize=12)
        plt.ylabel("Speed (mi/h)", fontsize=12)
        plt.title(name + " Confidence Band Plot Seg: " + str(i + 1) + " XD: " + str(xd_id_list[i]))
        plt.grid()
        plt.ylim(0, 80)
        plt.xticks(df_print[xaxis], bins.astype('int64'))
        plt.fill_between(df_print[xaxis], df_print['speed_' + str(lower_df) + 'th'],
                         df_print['speed_' + str(upper_df) + 'th'], alpha=0.2)
        plt.savefig(output + name + "\\" + name + "_" + str(lower_df) + "_" + str(upper_df) + "_" + str(i + 1) + ".png",
                    bbox_inches='tight', pad_inches=0.25)
        plt.close()
    return df_perc


def create_box_whisker_weekly(df, xd_id_list, output, lower_df, upper_df, name):
    xaxis = "start_date"
    df_perc = df.groupby(["xd_id", xaxis]).agg({"speed": [percentile(lower_df), percentile(upper_df), "median"]})
    df_perc.columns = df_perc.columns.map('|'.join).str.strip('|')
    df_perc.reset_index(inplace=True)
    df_perc = df_perc.rename(columns={'speed|percentile_' + str(lower_df): 'speed_' + str(lower_df) + 'th',
                                      'speed|percentile_' + str(upper_df): 'speed_' + str(upper_df) + 'th',
                                      'speed|median': 'speed_median'})

    colors = ['#e6194B', '#3cb44b', '#ffe119', '#4363d8', '#f58231', '#911eb4', '#42d4f4', '#f032e6', '#bfef45', '#fabed4', '#469990', '#dcbeff']
    df_perc["start_date"] = pd.to_datetime(df_perc["start_date"])
    df_perc["start_date_month"] = df_perc["start_date"].dt.month

    print("Creating " + name + " Box-Whiskers Plot ...")
    for i in range(0, segments):  # segments instead of 1
        df_print = df_perc[df_perc["xd_id"] == xd_id_list[i]]
        df_print.reset_index(inplace=True)

        box_colors = []
        for k in range(len(df_print["start_date_month"])):
            # print()
            box_colors.append(colors[df_print["start_date_month"][k]-1])

        # bins = np.linspace(1, len(df_print), num=len(df_print))
        fig = plt.figure(figsize=(6, 12))
        ax = fig.add_subplot(111)
        box_data = []
        for j in range(len(df_print)):
            temp_box_data = []
            temp_box_data.append(df_print['speed_' + str(lower_df) + 'th'][j])
            temp_box_data.append(df_print["speed_median"][j])
            temp_box_data.append(df_print['speed_' + str(upper_df) + 'th'][j])
            box_data.append(temp_box_data)

        bp = ax.boxplot(box_data, vert=0, patch_artist=True)
        for patch, color in zip(bp['boxes'], box_colors):
            patch.set_facecolor(color)

        for median in bp['medians']:
            median.set(color='black',
                       linewidth=1.5)

        for whisker in bp['whiskers']:
            whisker.set(color='black',
                       linewidth=1.5)

        plt.legend(loc="best", fontsize=8)
        plt.ylabel("Date", fontsize=12)
        plt.xlabel("Speed (mi/h)", fontsize=12)
        plt.title(name + " Box-Whisker Plot Seg: " + str(i + 1) + " XD: " + str(xd_id_list[i]))
        plt.grid()
        plt.xlim(0, 80)
        ytics = df_print[xaxis].astype(str)
        ax.set_yticklabels(ytics)
        ax.get_yaxis().tick_left()
        ax.legend(["15th|---Median---|85th %ile"])
        plt.savefig(output + name + "\\" + name + "_" + str(lower_df) + "_" + str(upper_df) + "_" + str(i + 1) + ".png",
                    bbox_inches='tight', pad_inches=0.25)
        plt.close()


    return df_perc


if __name__ == '__main__':
    print(1)
    ip_data_file, ip_id_file, xd_id, op_path, segments = input_file_path() # read path of the file
    print(2)
    ip_id_df, ip_df = read_input_file(ip_id_file, ip_data_file, xd_id)
    print(3)
    xd_id = ip_id_df["xd"].tolist()
    segments = len(xd_id)
    print(4)
    df_agg, df_agg_dt_hr = data_agg(ip_id_df, ip_df, idx="date", col="hour", pivot=False) # aggregating the data
    df_test_1 = df_agg_dt_hr.describe()
    print ("Data Imported")
    # Speed Heatmaps------------------------------------------------------------------------------------------
    create_heatmap(df_agg_dt_hr, xd_id, op_path, min_lim=10, max_lim=60, cmap="RdYlGn", value="speed", xticks=10,
                   cmap_label="Speed (mi/h)")

    # Speed Difference------------------------------------------------------------------------------------------
    create_heatmap(df_agg_dt_hr, xd_id, op_path, min_lim=0, max_lim=15, cmap="cool", value="speed_diff_85", xticks=10,
                   cmap_label="85th %ile Speed - Measured Speed (mi/h)") #85th %ile Speed - Measured Speed (mi/h), this was used for the NCHRP report

    # Travel Time Index--------------------------------------------------------------------------------------
    df_tti_15 = create_tti(df_agg_dt_hr, xd_id, op_path, "tti_15", "Travel Time")

    # Confidence Band Plots------------------------------------------------------------------------------------------
    df_speedcb_15_85 = create_confidence_band_hourly(ip_df, xd_id, op_path, 15, 85, "Speed_cb") # confidence bands with 15-85 percentile

    # Box whisker Plots------------------------------------------------------------------------------------------
    df_speedbw_15_85 = create_box_whisker_weekly(ip_df, xd_id, op_path, 15, 85, "Speed_bw") # Box whisker plots with 15- 85 percentile limits

